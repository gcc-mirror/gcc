from sarif import *

import pytest

@pytest.fixture(scope='function', autouse=True)
def sarif():
    return sarif_from_env()

def test_deletion_fixit(sarif):
    runs = sarif['runs']
    run = runs[0]
    results = run['results']

    # We expect a single error with a secondary location and a fix-it hint.
    #
    # The textual form of the diagnostic would look like this:
    #  . PATH/extra-semicolon.c:8:13: warning: extra semicolon in struct or union specified [-Wpedantic]
    #  .     8 |   int color;;
    #  .       |             ^
    #  .       |             -
    assert len(results) == 1
    
    result = results[0]
    assert result['level'] == 'warning'
    assert result['message']['text'] == "extra semicolon in struct or union specified"

    # We expect one fix-it hint representing a deletion of ';'
    assert len(result['fixes']) == 1
    fix = result['fixes'][0]
    assert fix['description']['text'] == "Delete ';'"
    assert len(fix['artifactChanges']) == 1
    change = fix['artifactChanges'][0]
    replacement = change['replacements'][0]
    assert replacement['deletedRegion']['startLine'] == 8
    assert replacement['deletedRegion']['startColumn'] == 13
    assert replacement['deletedRegion']['endColumn'] == 14
    assert replacement['insertedContent']['text'] == ''
