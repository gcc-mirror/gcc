from sarif import *

import pytest

@pytest.fixture(scope='function', autouse=True)
def sarif():
    return sarif_from_env()

def test_message_in_generated_sarif(sarif):
    result = get_result_by_index(sarif, 0)
    assert result['level'] == 'error'
    assert result['message']['text'] == "this is a string; foo; int: 42 str: mostly harmless; [this is a link](https://example.com/) 'this is quoted' highlight A highlight B (1)."
