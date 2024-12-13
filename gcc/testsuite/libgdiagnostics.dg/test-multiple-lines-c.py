from sarif import *

import pytest

@pytest.fixture(scope='function', autouse=True)
def sarif():
    return sarif_from_env()

def test_sarif_output(sarif):
    schema = sarif['$schema']
    assert schema == 'https://docs.oasis-open.org/sarif/sarif/v2.1.0/errata01/os/schemas/sarif-schema-2.1.0.json'

    version = sarif['version']
    assert version == '2.1.0'

    runs = sarif['runs']
    run = runs[0]

    tool = run['tool']
    assert tool['driver']['name'] == 'test-multiple-lines.c.exe'

    results = run['results']
    assert len(results) == 1
    result = results[0]
    assert result['ruleId'] == 'warning'
    assert result['level'] == 'warning'
    assert result['message']['text'] == "missing comma"
    assert len(result['locations']) == 1

    # The primary location should be that of the missing comma
    location = result['locations'][0]
    phys_loc = location['physicalLocation']
    assert phys_loc['artifactLocation']['uri'].endswith('test-multiple-lines.c')
    assert phys_loc['region']['startLine'] == 23
    assert phys_loc['region']['startColumn'] == 29
    assert phys_loc['region']['endColumn'] == 30
    assert phys_loc['contextRegion']['startLine'] == 23
    assert phys_loc['contextRegion']['snippet']['text'] \
        == '                       "bar"\n'

    assert len(location['relationships']) == 3
    location['relationships'][0]['target'] == 0
    location['relationships'][0]['kinds'] == ['relevant']
    location['relationships'][1]['target'] == 1
    location['relationships'][1]['kinds'] == ['relevant']
    location['relationships'][2]['target'] == 2
    location['relationships'][2]['kinds'] == ['relevant']

    # We should be capturing the secondary locations in relatedLocations
    assert len(result['relatedLocations']) == 3

    rel_loc_0 = result['relatedLocations'][0]
    assert get_location_artifact_uri(rel_loc_0) \
        .endswith('test-multiple-lines.c')
    assert get_location_snippet_text(rel_loc_0) \
        == 'const char *strs[3] = {"foo",\n'
    assert get_location_physical_region(rel_loc_0)['startLine'] == 22
    assert get_location_physical_region(rel_loc_0)['startColumn'] == 24
    assert get_location_physical_region(rel_loc_0)['endColumn'] == 29
    assert rel_loc_0['id'] == 0
    assert 'relationships' not in rel_loc_0

    rel_loc_1 = result['relatedLocations'][1]
    assert get_location_artifact_uri(rel_loc_1) \
        .endswith('test-multiple-lines.c')
    assert get_location_snippet_text(rel_loc_1) \
        == '                       "bar"\n'
    assert get_location_physical_region(rel_loc_1)['startLine'] == 23
    assert get_location_physical_region(rel_loc_1)['startColumn'] == 24
    assert get_location_physical_region(rel_loc_1)['endColumn'] == 29
    assert rel_loc_1['id'] == 1
    assert 'relationships' not in rel_loc_1

    rel_loc_2 = result['relatedLocations'][2]
    assert get_location_artifact_uri(rel_loc_2) \
        .endswith('test-multiple-lines.c')
    assert get_location_snippet_text(rel_loc_2) \
        == '                       "baz"};\n'
    assert get_location_physical_region(rel_loc_2)['startLine'] == 24
    assert get_location_physical_region(rel_loc_2)['startColumn'] == 24
    assert get_location_physical_region(rel_loc_2)['endColumn'] == 29
    assert rel_loc_2['id'] == 2
    assert 'relationships' not in rel_loc_2
