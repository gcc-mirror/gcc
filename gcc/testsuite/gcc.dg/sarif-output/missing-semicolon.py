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

    # We expect the 'error' to make executionSuccessful be false
    assert invocation['executionSuccessful'] == False

def test_location_relationships(sarif):
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

    location = locations[0]
    assert get_location_artifact_uri(location).endswith('missing-semicolon.c')
    assert get_location_snippet_text(location) == '  return 42\n'
    assert get_location_physical_region(location)['startLine'] == 9
    assert get_location_physical_region(location)['startColumn'] == 12
    assert get_location_physical_region(location)['endColumn'] == 13

    # We don't expect the secondary location to have a relationship back
    # to the primary location, and so the primary doesn't get an id.
    assert 'id' not in location

    # We expect the primary location to reference the secondary location.
    assert len(location['relationships']) == 1
    assert location['relationships'][0]['target'] == 0
    assert location['relationships'][0]['kinds'] == ['relevant']

    # We expect one related location, for the closing brace on the next line
    relatedLocations = result['relatedLocations']
    assert len(relatedLocations) == 1

    rel_loc = relatedLocations[0]
    assert rel_loc['id'] == 0
    assert get_location_artifact_uri(rel_loc).endswith('missing-semicolon.c')
    assert get_location_snippet_text(rel_loc) == '}\n'
    assert get_location_physical_region(rel_loc)['startLine'] == 10
    assert get_location_physical_region(rel_loc)['startColumn'] == 1
    assert get_location_physical_region(rel_loc)['endColumn'] == 2
    assert 'relatedLocations' not in rel_loc
    assert 'message' not in rel_loc

    # We expect one fix-it hint representing an insertion of ';'
    assert len(result['fixes']) == 1
    assert len(result['fixes'][0]['artifactChanges']) == 1
    change = result['fixes'][0]['artifactChanges'][0]
    assert change['artifactLocation']['uri'].endswith('missing-semicolon.c')
    assert len(change['replacements']) == 1
    replacement = change['replacements'][0]
    assert replacement['deletedRegion']['startLine'] == 9
    assert replacement['deletedRegion']['startColumn'] == 12
    assert replacement['deletedRegion']['endColumn'] == 12
    assert replacement['insertedContent']['text'] == ';'
