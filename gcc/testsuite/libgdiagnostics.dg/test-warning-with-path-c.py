from sarif import *

import pytest

@pytest.fixture(scope='function', autouse=True)
def sarif():
    return sarif_from_env()

final_line_num = 34

line_num_call_to_PyList_New = final_line_num - 7;
line_num_for_loop = final_line_num - 5;
line_num_call_to_PyList_Append = final_line_num - 3;

expected_file_name = 'test-warning-with-path.c'

def test_sarif_output_for_warning_with_path(sarif):
    schema = sarif['$schema']
    assert schema == 'https://docs.oasis-open.org/sarif/sarif/v2.1.0/errata01/os/schemas/sarif-schema-2.1.0.json'

    version = sarif['version']
    assert version == '2.1.0'

    runs = sarif['runs']
    run = runs[0]

    tool = run['tool']
    assert tool['driver']['name'] == expected_file_name + '.exe'

    results = run['results']
    assert len(results) == 1

    result = results[0]
    assert result['ruleId'] == 'warning'
    assert result['level'] == 'warning'
    assert result['message']['text'] \
        == "passing NULL as argument 1 to 'PyList_Append' which requires a non-NULL parameter"
    assert len(result['locations']) == 1
    location = result['locations'][0]

    phys_loc = location['physicalLocation']
    assert phys_loc['artifactLocation']['uri'].endswith(expected_file_name)
    assert phys_loc['region']['startLine'] \
        == line_num_call_to_PyList_Append
    assert phys_loc['region']['startColumn'] == 5
    assert phys_loc['region']['endColumn'] == 30
    assert phys_loc['contextRegion']['startLine'] \
        == line_num_call_to_PyList_Append
    assert phys_loc['contextRegion']['snippet']['text'] \
        == '    PyList_Append(list, item);\n'

    assert len(location['logicalLocations']) == 1
    logical_loc = location['logicalLocations'][0]
    assert logical_loc['name'] == 'make_a_list_of_random_ints_badly'
    assert logical_loc['fullyQualifiedName'] == 'make_a_list_of_random_ints_badly'
    assert logical_loc['decoratedName'] == 'make_a_list_of_random_ints_badly'
    assert logical_loc['kind'] == 'function'

    assert len(result['codeFlows']) == 1
    assert len(result['codeFlows'][0]['threadFlows']) == 1
    thread_flow = result['codeFlows'][0]['threadFlows'][0]

    assert len(thread_flow['locations']) == 3

    tfl_0 = thread_flow['locations'][0]
    tfl_0_loc = tfl_0['location']
    assert get_location_artifact_uri(tfl_0_loc).endswith(expected_file_name)
    assert get_location_physical_region(tfl_0_loc)['startLine'] \
        == line_num_call_to_PyList_New
    assert get_location_physical_region(tfl_0_loc)['startColumn'] == 10
    assert get_location_physical_region(tfl_0_loc)['endColumn'] == 23
    assert get_location_snippet_text(tfl_0_loc) \
        == '  list = PyList_New(0);\n'
    assert tfl_0_loc['logicalLocations'] == location['logicalLocations']
    assert tfl_0_loc['message']['text'] \
        == "when 'PyList_New' fails, returning NULL"
    assert tfl_0['nestingLevel'] == 0
    assert tfl_0['executionOrder'] == 1

    tfl_1 = thread_flow['locations'][1]
    tfl_1_loc = tfl_1['location']
    assert get_location_artifact_uri(tfl_1_loc).endswith(expected_file_name)
    assert get_location_physical_region(tfl_1_loc)['startLine'] \
        == line_num_for_loop
    assert get_location_physical_region(tfl_1_loc)['startColumn'] == 15
    assert get_location_physical_region(tfl_1_loc)['endColumn'] == 24
    assert get_location_snippet_text(tfl_1_loc) \
        == '  for (i = 0; i < count; i++) {\n'
    assert tfl_1_loc['logicalLocations'] == location['logicalLocations']
    assert tfl_1_loc['message']['text'] \
        == "when 'i < count'"
    assert tfl_1['nestingLevel'] == 0
    assert tfl_1['executionOrder'] == 2

    tfl_2 = thread_flow['locations'][2]
    tfl_2_loc = tfl_2['location']
    assert get_location_artifact_uri(tfl_2_loc).endswith(expected_file_name)
    assert get_location_physical_region(tfl_2_loc)['startLine'] \
        == line_num_call_to_PyList_Append
    assert get_location_physical_region(tfl_2_loc)['startColumn'] == 5
    assert get_location_physical_region(tfl_2_loc)['endColumn'] == 30
    assert get_location_snippet_text(tfl_2_loc) \
        == '    PyList_Append(list, item);\n'
    assert tfl_2_loc['logicalLocations'] == location['logicalLocations']
    assert tfl_2_loc['message']['text'] \
        == "when calling 'PyList_Append', passing NULL from (1) as argument 1"
    assert tfl_2['nestingLevel'] == 0
    assert tfl_2['executionOrder'] == 3
