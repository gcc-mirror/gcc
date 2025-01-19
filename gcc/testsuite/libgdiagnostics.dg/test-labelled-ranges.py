# Verify the SARIF output of test-labelled-ranges.{c,cc}

from sarif import *

import pytest

@pytest.fixture(scope='function', autouse=True)
def sarif():
    return sarif_from_env()

def test_sarif_output(sarif):
    schema = sarif['$schema']
    assert schema == 'https://docs.oasis-open.org/sarif/sarif/v2.1.0/errata01/os/schemas/sarif-schema-2.1.0.json'

    version = sarif['version']
    assert version == '2.1.0'

    runs = sarif['runs']
    run = runs[0]

    results = run['results']
    assert len(results) == 1
    assert results[0]['ruleId'] == 'error'
    assert results[0]['level'] == 'error'
    assert results[0]['message']['text'] \
        == "mismatching types: 'int' and 'const char *'"
    assert len(results[0]['locations']) == 1
    location = results[0]['locations'][0]
    phys_loc = location['physicalLocation']
    assert phys_loc['region']['startLine'] == 19
    assert phys_loc['region']['startColumn'] == 6
    assert phys_loc['region']['endColumn'] == 7
    assert phys_loc['contextRegion']['startLine'] == 19
    assert phys_loc['contextRegion']['snippet']['text'] \
        == '  42 + "foo"\n'

    annotations = location['annotations']
    assert len(annotations) == 2

    assert annotations[0]['startLine'] == 19
    assert annotations[0]['startColumn'] == 3
    assert annotations[0]['endColumn'] == 5
    assert annotations[0]['message']['text'] == 'int'

    assert annotations[1]['startLine'] == 19
    assert annotations[1]['startColumn'] == 8
    assert annotations[1]['endColumn'] == 13
    assert annotations[1]['message']['text'] == 'const char *'
