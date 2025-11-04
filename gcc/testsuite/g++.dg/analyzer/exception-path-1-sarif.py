from sarif import *

import pytest

@pytest.fixture(scope='function', autouse=True)
def sarif():
    return sarif_from_env()

def test_kinds(sarif):
    result = get_result_by_index(sarif, 0)

    assert result['level'] == 'note'

    events = result["codeFlows"][0]["threadFlows"][0]['locations']

    # Event "(1)": "throwing exception of type 'value_error' here..." (index == 0)
    assert events[0]['location']['message']['text'] == "throwing exception of type 'value_error' here..."
    assert events[0]['kinds'] == ["throw"]

    # Event "(2)": "...catching exception of type 'value_error' here" (index == 1)
    assert events[1]['location']['message']['text'] == "...catching exception of type 'value_error' here"
    assert events[1]['kinds'] == ["catch"]
