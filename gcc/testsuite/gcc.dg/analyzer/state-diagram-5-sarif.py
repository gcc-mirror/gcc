from sarif import *

import pytest

@pytest.fixture(scope='function', autouse=True)
def sarif():
    return sarif_from_env()

def test_nested_types_in_state_graph(sarif):
    result = get_result_by_index(sarif, 0)

    assert result['level'] == 'note'

    events = result["codeFlows"][0]["threadFlows"][0]['locations']

    assert events[0]['location']['message']['text'] == 'here'
    state = get_state_graph(events, 0)

    stack = state['nodes'][0]
    assert stack['id'] == 'stack'
    assert get_state_node_kind(stack) == 'stack'

    frame = stack['children'][0]
    assert frame['id'].startswith('frame-region-')
    assert get_state_node_kind(frame) == 'stack-frame'
    assert get_state_node_attr(frame, 'function') == 'test'
    assert frame['location']['logicalLocations'][0]['fullyQualifiedName'] == 'test'

    # We have:
    #   baz_arr[1].m_bars[1].m_foos[2].m_ints[1] = 42;

    # Verify that we correctly expand from the analyzer's bit-offset
    # representation to nested elements and fields.

    # "baz_arr":
    baz_arr = frame['children'][0]
    assert get_state_node_kind(baz_arr) == 'variable'
    assert get_state_node_type(baz_arr) == 'struct baz[2]'

    assert len(baz_arr['children']) == 2

    bindings = baz_arr['children'][0]
    assert bindings['id'].startswith('concrete-bindings-')
    assert get_state_node_kind(bindings) == 'other'
    assert get_state_node_value(bindings['children'][0]) == '(int)42'

    # "baz_arr[1]":
    baz_arr_1 = baz_arr['children'][1]
    assert get_state_node_type(baz_arr_1) == 'struct baz'
    assert get_state_node_kind(baz_arr_1) == 'element'
    assert get_state_node_attr(baz_arr_1, 'index') == '1'

    assert len(baz_arr_1['children']) == 1

    # "baz_arr[1].m_bars":
    baz_arr_1_m_bars = baz_arr_1['children'][0]
    assert get_state_node_name(baz_arr_1_m_bars) == 'm_bars'
    assert get_state_node_type(baz_arr_1_m_bars) == 'struct bar[2]'

    # "baz_arr[1].m_bars[1]"
    baz_arr_1_m_bars_1 = baz_arr_1_m_bars['children'][0]
    assert get_state_node_type(baz_arr_1_m_bars_1) == 'struct bar'
    assert get_state_node_kind(baz_arr_1_m_bars_1) == 'element'
    assert get_state_node_attr(baz_arr_1_m_bars_1, 'index') == '1'

    # "baz_arr[1].m_bars[1].m_foos"
    baz_arr_1_m_bars_1_m_foos = baz_arr_1_m_bars_1['children'][0]
    assert get_state_node_kind(baz_arr_1_m_bars_1_m_foos) == 'field'
    assert get_state_node_name(baz_arr_1_m_bars_1_m_foos) == 'm_foos'
    assert get_state_node_type(baz_arr_1_m_bars_1_m_foos) == 'struct foo[3]'

    # "baz_arr[1].m_bars[1].m_foos[2]"
    baz_arr_1_m_bars_1_m_foos_2 = baz_arr_1_m_bars_1_m_foos['children'][0]
    assert get_state_node_type(baz_arr_1_m_bars_1_m_foos_2) == 'struct foo'
    assert get_state_node_kind(baz_arr_1_m_bars_1_m_foos_2) == 'element'
    assert get_state_node_attr(baz_arr_1_m_bars_1_m_foos_2, 'index') == '2'

    # "baz_arr[1].m_bars[1].m_foos[2].m_ints"
    baz_arr_1_m_bars_1_m_foos_2_m_ints = baz_arr_1_m_bars_1_m_foos_2['children'][0]
    assert get_state_node_kind(baz_arr_1_m_bars_1_m_foos_2_m_ints) == 'field'
    assert get_state_node_name(baz_arr_1_m_bars_1_m_foos_2_m_ints) == 'm_ints'
    assert get_state_node_type(baz_arr_1_m_bars_1_m_foos_2_m_ints) == 'int[4]'

    # "baz_arr[1].m_bars[1].m_foos[2].m_ints[1]"
    baz_arr_1_m_bars_1_m_foos_2_m_ints_1 = baz_arr_1_m_bars_1_m_foos_2_m_ints['children'][0]
    assert get_state_node_type(baz_arr_1_m_bars_1_m_foos_2_m_ints_1) == 'int'
    assert get_state_node_kind(baz_arr_1_m_bars_1_m_foos_2_m_ints_1) == 'element'
    assert get_state_node_attr(baz_arr_1_m_bars_1_m_foos_2_m_ints_1, 'index') == '1'
    assert get_state_node_value(baz_arr_1_m_bars_1_m_foos_2_m_ints_1) == '(int)42'
