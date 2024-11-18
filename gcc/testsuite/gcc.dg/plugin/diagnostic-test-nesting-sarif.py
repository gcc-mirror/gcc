from sarif import *

import pytest

@pytest.fixture(scope='function', autouse=True)
def sarif():
    return sarif_from_env()

def test_basics(sarif):
    schema = sarif['$schema']
    assert schema == "https://docs.oasis-open.org/sarif/sarif/v2.1.0/errata01/os/schemas/sarif-schema-2.1.0.json"

    version = sarif['version']
    assert version == "2.1.0"

def test_nested_result(sarif):
    runs = sarif['runs']
    run = runs[0]
    results = run['results']

    assert len(results) == 1

    result = results[0]
    assert result['level'] == 'error'
    assert result['message']['text'] == "top-level error"

    relatedLocations = result['relatedLocations']
    assert len(relatedLocations) == 12

    for i in range(12):
        note = relatedLocations[i]
        text = note['message']['text']
        nestingLevel = note['properties']['nestingLevel']
        if i % 4 == 0:
            assert text == 'child %i' % (i / 4)
            assert nestingLevel == 1
        else:
            assert text == 'grandchild %i %i' % ((i / 4), (i % 4) - 1)
            assert nestingLevel == 2
