# We expect an aanalyzer warning with a path, referencing the #include
# both in the warning and in the events within its execution path.
# In textual form, we'd expect something like:
# . In file included from PATH/include-chain-2.c:28:
# . PATH/include-chain-2.h: In function 'test':
# . PATH/include-chain-2.h:6:3: warning: double-'free' of 'ptr' [CWE-415] [-Wanalyzer-double-free]
# .     6 |   __builtin_free (ptr);
# .       |   ^~~~~~~~~~~~~~~~~~~~
# .   'test': events 1-2
# .     5 |   __builtin_free (ptr);
# .       |   ^~~~~~~~~~~~~~~~~~~~
# .       |   |
# .       |   (1) first 'free' here
# .     6 |   __builtin_free (ptr);
# .       |   ~~~~~~~~~~~~~~~~~~~~
# .       |   |
# .       |   (2) second 'free' here; first 'free' was at (1)

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

    # We expect a mere 'warning' to allow executionSuccessful be true
    assert invocation['executionSuccessful'] == True

def test_result(sarif):
    runs = sarif['runs']
    run = runs[0]
    results = run['results']

    assert len(results) == 1
    
    result = results[0]
    assert result['ruleId'] == '-Wanalyzer-double-free'
    assert result['level'] == 'warning'
    assert result['message']['text'] == "double-'free' of 'ptr'"
    assert result["taxa"] == [{"id": "415",
                               "toolComponent": {"name": "cwe"}}]

def test_location_relationships(sarif):
    runs = sarif['runs']
    run = runs[0]
    results = run['results']

    assert len(results) == 1
    
    result = results[0]
    locations = result['locations']
    assert len(locations) == 1

    location = locations[0]
    assert 'id' in location
    assert get_location_artifact_uri(location).endswith('include-chain-2.h')
    assert get_location_snippet_text(location) == "  __builtin_free (ptr); // 2nd\n"

    # We expect one related location, describing the #include
    relatedLocations = result['relatedLocations']
    assert len(relatedLocations) == 1

    # The "#include "include-chain-1.h" line in include-chain-1.c:
    hash_include_2_h = relatedLocations[0]
    assert 'id' in hash_include_2_h
    assert get_location_snippet_text(hash_include_2_h) == '#include "include-chain-2.h"\n'
    assert get_location_artifact_uri(hash_include_2_h).endswith('include-chain-2.c')

    # We expect an execution path
    assert len(result['codeFlows']) == 1
    codeFlow = result['codeFlows'][0]
    assert len(codeFlow['threadFlows']) == 1
    threadFlow = codeFlow['threadFlows'][0]
    assert threadFlow['id'] == 'main'
    assert len(threadFlow['locations']) == 2
    
    assert threadFlow['locations'][0]['location']['message']['text'] \
        == "first 'free' here"
    assert threadFlow['locations'][0]['location']['physicalLocation']['contextRegion']['snippet']['text'] \
        == "  __builtin_free (ptr); // 1st\n"
    assert threadFlow['locations'][0]['kinds'] == ['release', 'memory']
    assert threadFlow['locations'][0]['executionOrder'] == 1

    # We should have an embedded link in this event's message to the
    # other event's location within the SARIF file:
    assert threadFlow['locations'][1]['location']['message']['text'] \
        == "second 'free' here; first 'free' was at [(1)](sarif:/runs/0/results/0/codeFlows/0/threadFlows/0/locations/0)"
    assert threadFlow['locations'][1]['location']['physicalLocation']['contextRegion']['snippet']['text'] \
        == "  __builtin_free (ptr); // 2nd\n"
    assert threadFlow['locations'][1]['kinds'] == ['danger']
    assert threadFlow['locations'][1]['executionOrder'] == 2

    # Check the various relationships; we expect a directed graph of edges
    # representing the various "isIncludedBy" and "includes" relationships.

    # The primary location should be "isIncludedBy" the "#include "include-chain-2.h" line
    assert len(location['relationships']) == 1
    assert location['relationships'][0]['target'] == hash_include_2_h['id']
    assert location['relationships'][0]['kinds'] == ["isIncludedBy"]

    # Similarly, so should the locations within the threadFlow
    event0_rels = get_location_relationships(threadFlow['locations'][0]['location'])
    assert len(event0_rels) == 1
    assert event0_rels[0]['target'] == hash_include_2_h['id']
    assert event0_rels[0]['kinds'] == ["isIncludedBy"]

    event1_rels = get_location_relationships(threadFlow['locations'][1]['location'])
    assert len(event1_rels) == 1
    assert event1_rels[0]['target'] == hash_include_2_h['id']
    assert event1_rels[0]['kinds'] == ["isIncludedBy"]

    # The "#include "include-chain-2.h" line in include-chain-2.c should
    # have an "includes" relationship to the main location and to the
    # two locations in the execution path:
    assert len(hash_include_2_h['relationships']) == 3
    assert hash_include_2_h['relationships'][0]['target'] == location['id']
    assert hash_include_2_h['relationships'][0]['kinds'] == ["includes"]
    assert hash_include_2_h['relationships'][1]['target'] == threadFlow['locations'][0]['location']['id']
    assert hash_include_2_h['relationships'][1]['kinds'] == ["includes"]
    assert hash_include_2_h['relationships'][2]['target'] == threadFlow['locations'][1]['location']['id']
    assert hash_include_2_h['relationships'][2]['kinds'] == ["includes"]
