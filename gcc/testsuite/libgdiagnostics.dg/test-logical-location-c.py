from sarif import *

import pytest

@pytest.fixture(scope='function', autouse=True)
def sarif():
    return sarif_from_env()

def test_sarif_output_with_logical_location(sarif):
    schema = sarif['$schema']
    assert schema == 'https://docs.oasis-open.org/sarif/sarif/v2.1.0/errata01/os/schemas/sarif-schema-2.1.0.json'

    version = sarif['version']
    assert version == '2.1.0'

    runs = sarif['runs']
    run = runs[0]

    tool = run['tool']
    assert tool['driver']['name'] == 'test-logical-location.c.exe'

    results = run['results']
    assert len(results) == 1

    result = results[0]
    assert result['ruleId'] == 'error'
    assert result['level'] == 'error'
    assert result['message']['text'] == "can't find 'foo'"
    assert len(result['locations']) == 1
    location = result['locations'][0]

    assert len(location['logicalLocations']) == 1
    logical_loc = location['logicalLocations'][0]
    assert logical_loc['name'] == 'test_short_name'
    assert logical_loc['fullyQualifiedName'] == 'test_qualified_name'
    assert logical_loc['decoratedName'] == 'test_decorated_name'
    assert logical_loc['kind'] == 'function'
