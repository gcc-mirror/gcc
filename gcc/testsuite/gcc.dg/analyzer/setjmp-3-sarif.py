from sarif import *

import pytest

@pytest.fixture(scope='function', autouse=True)
def sarif():
    return sarif_from_env()

def test_kinds(sarif):
    result = get_result_by_index(sarif, 0)

    assert result['level'] == 'note'

    events = result["codeFlows"][0]["threadFlows"][0]['locations']

    assert events[1]['location']['message']['text'] == "'setjmp' called here"
    assert events[1]['kinds'] == ["setjmp"]

    assert events[6]['location']['message']['text'] == "rewinding from 'longjmp' in 'inner'..."
    assert events[6]['kinds'] == ["longjmp"]

    assert events[7]['location']['message']['text'].startswith("...to 'setjmp' in 'outer'")
    assert events[7]['kinds'] == ["longjmp"]
