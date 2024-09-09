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
    assert invocation['toolExecutionNotifications'] == []

def test_empty_results(sarif):
    runs = sarif['runs']
    run = runs[0]
    results = run['results']
    assert len(results) == 0
