from sarif import *

import pytest

@pytest.fixture(scope='function', autouse=True)
def sarif():
    return sarif_from_env()

def test_kinds(sarif):
    result = get_result_by_index(sarif, 0)

    assert result['level'] == 'note'

    events = result["codeFlows"][0]["threadFlows"][0]['locations']

    assert events[-4]['location']['message']['text'] == "throwing exception of type 'value_error' here..."
    assert events[-4]['kinds'] == ["throw"]

    assert events[-3]['location']['message']['text'] == "unwinding 2 stack frames"
    assert events[-3]['kinds'] == ["unwind"]

    assert events[-2]['location']['message']['text'] == "...catching exception of type 'value_error' here"
    assert events[-2]['kinds'] == ["catch"]
