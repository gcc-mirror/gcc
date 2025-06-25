import xml.etree.ElementTree as ET

from sarif import *

import pytest

@pytest.fixture(scope='function', autouse=True)
def sarif():
    return sarif_from_env()

def test_nested_types_in_xml_state(sarif):
    result = get_result_by_index(sarif, 0)

    assert result['level'] == 'note'

    events = result["codeFlows"][0]["threadFlows"][0]['locations']

    assert events[0]['location']['message']['text'] == 'here'
    state = get_xml_state(events, 0)

    memory_regions = state.find('memory-regions')
    assert memory_regions is not None

    stack = memory_regions.find('stack')
    assert stack is not None

    frame = stack.find('stack-frame')
    assert frame.get('function') == 'test'

    # We have:
    #   baz_arr[1].m_bars[1].m_foos[2].m_ints[1] = 42;

    # Verify that we correctly expand from the analyzer's bit-offset
    # representation to nested elements and fields.

    # "baz_arr":
    baz_arr = frame.find("variable[@name='baz_arr']")
    assert baz_arr.get('type') == 'struct baz[2]'

    # "baz_arr[1]":
    baz_arr_1 = baz_arr.find("element[@index='1']")
    assert baz_arr_1.get('type') == 'struct baz'

    assert baz_arr.find("element[@index='0']") is None

    # "baz_arr[1].m_bars":
    baz_arr_1_m_bars = baz_arr_1.find("field[@name='m_bars']")
    assert baz_arr_1_m_bars.get('type') == 'struct bar[2]'

    # "baz_arr[1].m_bars[1]"
    baz_arr_1_m_bars_1 = baz_arr_1_m_bars.find("element[@index='1']")
    assert baz_arr_1_m_bars_1.get('type') == 'struct bar'

    # "baz_arr[1].m_bars[1].m_foos"
    baz_arr_1_m_bars_1_m_foos = baz_arr_1_m_bars_1.find("field[@name='m_foos']")
    assert baz_arr_1_m_bars_1_m_foos.get('type') == 'struct foo[3]'

    # "baz_arr[1].m_bars[1].m_foos[2]"
    baz_arr_1_m_bars_1_m_foos_2 = baz_arr_1_m_bars_1_m_foos.find("element[@index='2']")
    assert baz_arr_1_m_bars_1_m_foos_2.get('type') == 'struct foo'
    
    # "baz_arr[1].m_bars[1].m_foos[2].m_ints"
    baz_arr_1_m_bars_1_m_foos_2_m_ints = baz_arr_1_m_bars_1_m_foos_2.find('field[@name="m_ints"]')
    assert baz_arr_1_m_bars_1_m_foos_2_m_ints.get('type') == 'int[4]'
    
    # "baz_arr[1].m_bars[1].m_foos[2].m_ints[1]"
    baz_arr_1_m_bars_1_m_foos_2_m_ints_1 = baz_arr_1_m_bars_1_m_foos_2_m_ints.find('element[@index="1"]')
    assert baz_arr_1_m_bars_1_m_foos_2_m_ints_1.get('type') == 'int'

    value = baz_arr_1_m_bars_1_m_foos_2_m_ints_1.find('value-of-region')
    constant = value.find('constant')
    assert constant.get('value') == '42'
    assert constant.get('type') == 'int'
