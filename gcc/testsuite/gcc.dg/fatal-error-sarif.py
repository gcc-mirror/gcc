from sarif import *

import pytest

@pytest.fixture(scope='function', autouse=True)
def sarif():
    return sarif_from_env()

def test_execution_unsuccessful(sarif):
    runs = sarif['runs']
    run = runs[0]

    invocations = run['invocations']
    assert len(invocations) == 1
    invocation = invocations[0]

    # We expect the fatal error to make executionSuccessful be false
    assert invocation['executionSuccessful'] == False

def test_fatal_error(sarif):
    runs = sarif['runs']
    run = runs[0]
    results = run['results']

    assert len(results) == 1
    
    result = results[0]
    assert result['level'] == 'error'
    assert result['message']['text'] == "this-does-not-exist.h: No such file or directory"
