from sarif import *

import pytest

@pytest.fixture(scope='function', autouse=True)
def sarif():
    return sarif_from_env()

def test_basics(sarif):
    schema = sarif['$schema']
    assert schema == "https://docs.oasis-open.org/sarif/sarif/v2.1.0/errata01/os/schemas/sarif-schema-2.1.0.json"

    version = sarif['version']
    assert version == "2.1.0"

def test_execution_unsuccessful(sarif):
    runs = sarif['runs']
    run = runs[0]

    invocations = run['invocations']
    assert len(invocations) == 1
    invocation = invocations[0]

    assert '-Werror=unused-variable' in invocation['arguments']

    # We expect the 'Werror' to make executionSuccessful be false
    assert invocation['executionSuccessful'] == False

def test_result(sarif):
    runs = sarif['runs']
    run = runs[0]
    results = run['results']

    assert len(results) == 1
    
    result = results[0]
    assert result['ruleId'] == '-Werror=unused-variable'
    assert result['level'] == 'error'
    assert result['message']['text'] == "'ununsed' defined but not used"
