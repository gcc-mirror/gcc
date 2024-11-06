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

    # We expect the 'error' to make executionSuccessful be false
    assert invocation['executionSuccessful'] == False

def test_result(sarif):
    runs = sarif['runs']
    run = runs[0]
    results = run['results']

    # We expect a single error with a secondary location and a fix-it hint.
    #
    # The textual form of the diagnostic would look like this:
    #  . PATH/missing-semicolon.c: In function 'missing_semicolon':
    #  . PATH/missing-semicolon.c:19:12: error: expected ';' before '}' token
    #  .    19 |   return 42
    #  .       |            ^
    #  .       |            ;
    #  .    20 | }
    #  .       | ~           
    assert len(results) == 1
    
    result = results[0]
    assert result['level'] == 'error'
    assert result['message']['text'] == "expected ';' before '}' token"
    locations = result['locations']
    assert len(locations) == 1

    assert len(result['fixes']) == 1
    assert len(result['fixes'][0]['artifactChanges']) == 1
    change = result['fixes'][0]['artifactChanges'][0]
    assert len(change['replacements']) == 1
    replacement = change['replacements'][0]
    assert replacement['deletedRegion']['startColumn'] == 12
    assert replacement['deletedRegion']['endColumn'] == 12
    assert replacement['insertedContent']['text'] == ';'
