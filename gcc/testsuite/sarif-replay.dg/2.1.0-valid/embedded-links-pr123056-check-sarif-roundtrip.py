from sarif import *

import pytest

@pytest.fixture(scope='function', autouse=True)
def sarif():
    return sarif_from_env()

def test_roundtrip_of_url_in_generated_sarif(sarif):
    result = get_result_by_index(sarif, 0)
    assert result['level'] == 'warning'
    assert result['message']['text'] == "leak of ‘p’"
    assert (result['codeFlows'][0]['threadFlows'][0]['locations'][1]['location']['message']['text']
            == "‘p’ leaks here; was allocated at (1)")
