from sarif import *

import pytest

@pytest.fixture(scope='function', autouse=True)
def sarif():
    return sarif_from_env()

def test_state_graph(sarif):
    result = get_result_by_index(sarif, 0)

    assert result['level'] == 'warning'
    assert result['ruleId'] == '-Wanalyzer-use-after-free'

    # TODO: check code flow
    events = result["codeFlows"][0]["threadFlows"][0]['locations']

    # Event "(1)": "entry to 'test'" (index == 0)
    assert events[0]['location']['message']['text'] == "entry to 'test'"
    state0 = get_state_graph(events, 0)

    stack = state0['nodes'][0]
    assert stack['id'] == 'stack'
    assert get_state_node_kind(stack) == 'stack'

    frame = stack['children'][0]
    assert frame['id'].startswith('frame-region-')
    assert get_state_node_kind(frame) == 'stack-frame'
    assert get_state_node_attr(frame, 'function') == 'test'
    assert frame['location']['logicalLocations'][0]['fullyQualifiedName'] == 'test'

    # Final event:
    assert events[-1]['location']['message']['text'].startswith("use after 'free' of ")
    state = get_state_graph(events, -1)

    stack = state['nodes'][0]
    assert stack['id'] == 'stack'
    assert get_state_node_kind(stack) == 'stack'

    frame = stack['children'][0]
    assert frame['id'].startswith('frame-region-')
    assert get_state_node_kind(frame) == 'stack-frame'
    assert get_state_node_attr(frame, 'function') == 'test'
    assert frame['location']['logicalLocations'][0]['fullyQualifiedName'] == 'test'

    heap = state['nodes'][1]
    assert heap['id'] == 'heap'
    assert get_state_node_kind(heap) == 'heap'

    assert len(heap['children']) == 3
    heap_buffer0 = heap['children'][0]
    assert heap_buffer0['id'].startswith('heap-allocated-region-')
    assert get_state_node_kind(heap_buffer0) == 'dynalloc-buffer'

    globals_ = state['nodes'][2]
    assert globals_['id'] == 'globals'
    assert get_state_node_kind(globals_) == 'globals'

    first = globals_['children'][0]
    assert first['id'].startswith('decl-region-')
    assert get_state_node_kind(first) == 'variable'
    assert get_state_node_name(first) == 'first'
    assert get_state_node_type(first) == 'struct node *'
    
    assert len(state['edges']) == 4
