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

    # We expect the errors to make executionSuccessful be false
    assert invocation['executionSuccessful'] == False

def test_location_relationships(sarif):
    runs = sarif['runs']
    run = runs[0]
    results = run['results']

    # We expect a pair of errors, each with a note, and include chains.
    # The textual form of these four diagnostics would look like this:
    #  . In file included from PATH/include-chain-1.h:5,
    #  .                  from PATH/include-chain.c:9:
    #  . PATH/include-chain-1-2.h:1:6: error: conflicting types for 'p'; have 'char'
    #  .     1 | char p;
    #  .       |      ^
    #  . In file included from PATH/include-chain-1.h:2:
    #  . PATH/include-chain-1-1.h:1:5: note: previous declaration of 'p' with type 'int'
    #  .     1 | int p;
    #  .       |     ^
    #  . PATH/include-chain-1-2.h:2:6: error: conflicting types for 'q'; have 'char'
    #  .     2 | char q;
    #  .       |      ^
    #  . PATH/include-chain-1-1.h:2:5: note: previous declaration of 'q' with type 'int'
    #  .     2 | int q;
    #  .       |     ^
    assert len(results) == 2
    
    result = results[0]
    assert result['level'] == 'error'
    assert result['message']['text'] == "conflicting types for 'p'; have 'char'"
    locations = result['locations']
    assert len(locations) == 1

    location = locations[0]
    assert 'id' in location
    assert get_location_artifact_uri(location).endswith('include-chain-1-2.h')
    assert get_location_snippet_text(location) == "char p;\n"

    # We expect 4 related locations: one for the "note"
    # and three for describing include chains
    relatedLocations = result['relatedLocations']
    assert len(relatedLocations) == 4

    # We expect a related location representing the note:
    #  . In file included from PATH/include-chain-1.h:2:
    #  .                  from PATH/include-chain.c:9:
    #  . PATH/include-chain-1-1.h:1:5: note: previous declaration of 'p' with type 'int'
    #  .     1 | int p;
    #  .       |     ^
    note = relatedLocations[0]
    assert 'id' in note
    assert note['message']['text'] == "previous declaration of 'p' with type 'int'"
    assert get_location_artifact_uri(note).endswith('include-chain-1-1.h')
    assert get_location_snippet_text(note) == "int p;\n"

    # We expect three more related locations for the two include chains

    # The "#include "include-chain-1-2.h" line in include-chain-1.h:
    hash_include_1_2_h = relatedLocations[1]
    assert 'id' in hash_include_1_2_h
    assert get_location_snippet_text(hash_include_1_2_h) == '#include "include-chain-1-2.h"\n'
    assert get_location_artifact_uri(hash_include_1_2_h).endswith('include-chain-1.h')

    # The "#include "include-chain-1-1.h" line in include-chain-1.h:
    hash_include_1_1_h = relatedLocations[2]
    assert 'id' in hash_include_1_1_h
    assert get_location_snippet_text(hash_include_1_1_h) == '#include "include-chain-1-1.h"\n'
    assert get_location_artifact_uri(hash_include_1_1_h).endswith('include-chain-1.h')

    # The "#include "include-chain-1.h" line in include-chain-1.c:
    hash_include_1_h = relatedLocations[3]
    assert 'id' in hash_include_1_h
    assert get_location_snippet_text(hash_include_1_h) == '#include "include-chain-1.h"\n'
    assert get_location_artifact_uri(hash_include_1_h).endswith('include-chain-1.c')

    # Check the various relationships; we expect a directed graph of edges
    # representing the various "isIncludedBy" and "includes" relationships.

    # The primary location should be "isIncludedBy" the "#include "include-chain-1-2.h" line
    assert len(location['relationships']) == 1
    assert location['relationships'][0]['target'] == hash_include_1_2_h['id']
    assert location['relationships'][0]['kinds'] == ["isIncludedBy"]

    # The note should be "isIncludedBy" the "#include "include-chain-1-1.h" line
    assert len(note['relationships']) == 1
    assert note['relationships'][0]['target'] == hash_include_1_1_h['id']
    assert note['relationships'][0]['kinds'] == ["isIncludedBy"]

    # The "#include "include-chain-1-2.h" line:
    assert len(hash_include_1_2_h['relationships']) == 2
    assert hash_include_1_2_h['relationships'][0]['target'] == location['id']
    assert hash_include_1_2_h['relationships'][0]['kinds'] == ["includes"]
    assert hash_include_1_2_h['relationships'][1]['target'] == hash_include_1_h['id']
    assert hash_include_1_2_h['relationships'][1]['kinds'] == ["isIncludedBy"]
    
    # The "#include "include-chain-1-1.h" line:
    assert len(hash_include_1_1_h['relationships']) == 2
    assert hash_include_1_1_h['relationships'][0]['target'] == note['id']
    assert hash_include_1_1_h['relationships'][0]['kinds'] == ["includes"]
    assert hash_include_1_1_h['relationships'][1]['target'] == hash_include_1_h['id']
    assert hash_include_1_1_h['relationships'][1]['kinds'] == ["isIncludedBy"]
    
    # The "#include "include-chain-1.h" line in include-chain-1.c:
    assert len(hash_include_1_h['relationships']) == 2
    assert hash_include_1_h['relationships'][0]['target'] == hash_include_1_2_h['id']
    assert hash_include_1_h['relationships'][0]['kinds'] == ["includes"]
    assert hash_include_1_h['relationships'][1]['target'] == hash_include_1_1_h['id']
    assert hash_include_1_h['relationships'][1]['kinds'] == ["includes"]

    # We expect similar for the 2nd error
    assert results[1]['level'] == 'error'
    assert results[1]['message']['text'] == "conflicting types for 'q'; have 'char'"
    assert len(results[1]['relatedLocations']) == 4
