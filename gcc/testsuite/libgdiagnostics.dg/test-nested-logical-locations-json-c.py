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
    assert tool['driver']['name'] == 'test-nested-logical-locations-json.c.exe'

    results = run['results']
    assert len(results) == 2

    result = results[0]
    assert result['ruleId'] == 'warning'
    assert result['level'] == 'warning'
    assert result['message']['text'] == "product ID is blank"
    assert len(result['locations']) == 1
    location = result['locations'][0]
    assert len(location['logicalLocations']) == 1
    logical_loc = location['logicalLocations'][0]
    assert logical_loc['index'] == 3
    assert logical_loc['fullyQualifiedName'] == '/orders/0/productIds/1'

    result = results[1]
    assert result['ruleId'] == 'warning'
    assert result['level'] == 'warning'
    assert result['message']['text'] == "value is negative"
    assert len(result['locations']) == 1
    location = result['locations'][0]
    assert len(location['logicalLocations']) == 1
    logical_loc = location['logicalLocations'][0]
    assert logical_loc['index'] == 4
    assert logical_loc['fullyQualifiedName'] == '/orders/0/total'

    # Check theRun.logicalLocations
    assert 'logicalLocations' in run
    assert len(run['logicalLocations']) == 5
    logical_loc = run['logicalLocations'][0]
    assert logical_loc['name'] == 'orders'
    assert logical_loc['fullyQualifiedName'] == '/orders'
    assert logical_loc['kind'] == 'array'
    assert logical_loc['index'] == 0
    logical_loc = run['logicalLocations'][1]
    assert logical_loc['name'] == '0'
    assert logical_loc['fullyQualifiedName'] == '/orders/0'
    assert logical_loc['kind'] == 'object'
    assert logical_loc['parentIndex'] == 0
    assert logical_loc['index'] == 1
    logical_loc = run['logicalLocations'][2]
    assert logical_loc['name'] == 'productIds'
    assert logical_loc['fullyQualifiedName'] == '/orders/0/productIds'
    assert logical_loc['kind'] == 'array'
    assert logical_loc['parentIndex'] == 1
    assert logical_loc['index'] == 2
    logical_loc = run['logicalLocations'][3]
    assert logical_loc['name'] == '1'
    assert logical_loc['fullyQualifiedName'] == '/orders/0/productIds/1'
    assert logical_loc['kind'] == 'value'
    assert logical_loc['parentIndex'] == 2
    assert logical_loc['index'] == 3
    logical_loc = run['logicalLocations'][4]
    assert logical_loc['name'] == 'total'
    assert logical_loc['fullyQualifiedName'] == '/orders/0/total'
    assert logical_loc['kind'] == 'property'
    assert logical_loc['parentIndex'] == 1
    assert logical_loc['index'] == 4

