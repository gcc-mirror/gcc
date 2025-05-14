from sarif import *

import pytest

@pytest.fixture(scope='function', autouse=True)
def sarif():
    return sarif_from_env()

expected_filename = 'logical-locations-1.C'

def test_result(sarif):
    runs = sarif['runs']
    run = runs[0]
    results = run['results']

    # The textual form of the diagnostic would look like this:
    #  . PATH/logical-locations-1.C: In member function ‘void ns::foo::bar()’:
    #  . PATH/logical-locations-1.C:12:14: error: return-statement with a value, in function returning ‘void’ [-fpermissive]
    #  .    12 |       return 0;
    #  .       |              ^
    assert len(results) == 1

    result = results[0]
    assert result['ruleId'] == '-fpermissive'
    assert result['level'] == 'error'
    assert result['message']['text'] \
        == "return-statement with a value, in function returning 'void'"

    locations = result['locations']
    assert len(locations) == 1

    location = locations[0]
    assert get_location_artifact_uri(location).endswith(expected_filename)
    assert get_location_snippet_text(location) == '      return 0;\n'

def test_logical_locations(sarif):
    runs = sarif['runs']
    run = runs[0]

    # We expect 3 logical locations within the run:
    assert len(run['logicalLocations']) == 3

    assert run['logicalLocations'][0] \
        == {"name": "ns",
            "fullyQualifiedName": "ns",
            "kind": "namespace",
            "index": 0}
    assert run['logicalLocations'][1] \
        == {"name": "foo",
            # Ideally we'd also have:
            #   "fullyQualifiedName": "ns::foo",
            "kind": "type",
            "parentIndex": 0,
            "index": 1}
    assert run['logicalLocations'][2] \
        == {"name": "bar",
            "fullyQualifiedName": "ns::foo::bar",
            "decoratedName": "_ZN2ns3foo3barEv",
            "kind": "function",
            "parentIndex": 1,
            "index": 2}

    results = run['results']

    assert len(results) == 1

    result = results[0]

    locations = result['locations']
    assert len(locations) == 1

    location = locations[0]

    # We expect one logical location within the result, referencing
    # one in the run
    assert len(location['logicalLocations']) == 1
    assert location['logicalLocations'][0] \
        == {'fullyQualifiedName': 'ns::foo::bar',
            'index': 2}
