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

def test_execution_successful(sarif):
    runs = sarif['runs']
    run = runs[0]

    invocations = run['invocations']
    assert len(invocations) == 1
    invocation = invocations[0]

    assert invocation['executionSuccessful'] == True

def test_warning(sarif):
    result = get_result_by_index(sarif, 0)

    assert result['level'] == 'warning'

    # TODO: this should be "-Wanalyzer-unsafe-call-within-signal-handler" and have a URL
    assert result['ruleId'] == 'warning'

    # TODO: check code flow
    events = result["codeFlows"][0]["threadFlows"][0]['locations']

    # Event "(1)": "entry to 'main'" (index == 0)
    assert events[0]['location']['message']['text'] == "entry to ‘main’"

    # Final event:
    assert events[-1]['location']['message']['text'].startswith("call to ‘fprintf’ from within signal handler")
