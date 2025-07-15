from sarif import *

import pytest

@pytest.fixture(scope='function', autouse=True)
def sarif():
    return sarif_from_env()

def test_roundtrip_of_url_in_generated_sarif(sarif):
    # Get "warning: 004: This is a link."
    result = get_result_by_index(sarif, 3)
    assert result['level'] == 'warning'
    assert result['message']['text'] == "004: This is a [link](http://www.example.com)."
