from sarif import *

import pytest

@pytest.fixture(scope='function', autouse=True)
def sarif():
    return sarif_from_env()

def test_xml_state(sarif):
    result = get_result_by_index(sarif, 0)

    assert result['level'] == 'warning'
    assert result['ruleId'] == '-Wanalyzer-use-after-free'

    # TODO: check code flow
    events = result["codeFlows"][0]["threadFlows"][0]['locations']

    # Event "(1)": "entry to 'test'" (index == 0)
    assert events[0]['location']['message']['text'] == "entry to 'test'"
    state0 = get_xml_state(events, 0)
    memory_regions = state0.find('memory-regions')
    assert memory_regions is not None
    stack = memory_regions.find('stack')
    assert stack is not None
    frame = stack.find('stack-frame')
    assert frame.get('function') == 'test'

    # Final event:
    assert events[-1]['location']['message']['text'].startswith("use after 'free' of ")
    state = get_xml_state(events, -1)
    # TODO

