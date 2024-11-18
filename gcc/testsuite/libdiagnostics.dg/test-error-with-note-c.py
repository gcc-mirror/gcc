from sarif import *

import pytest

@pytest.fixture(scope='function', autouse=True)
def sarif():
    return sarif_from_env()

expected_line_num = 18

def test_sarif_output_for_note(sarif):
    schema = sarif['$schema']
    assert schema == 'https://docs.oasis-open.org/sarif/sarif/v2.1.0/errata01/os/schemas/sarif-schema-2.1.0.json'

    version = sarif['version']
    assert version == '2.1.0'

    runs = sarif['runs']
    run = runs[0]

    tool = run['tool']
    assert tool['driver']['name'] == 'test-error-with-note.c.exe'

    results = run['results']
    assert len(results) == 1
    assert results[0]['ruleId'] == 'error'
    assert results[0]['level'] == 'error'
    assert results[0]['message']['text'] == "can't find 'foo.h'"
    assert len(results[0]['locations']) == 1
    location = results[0]['locations'][0]
    phys_loc = location['physicalLocation']
    assert phys_loc['artifactLocation']['uri'].endswith('test-error-with-note.c')
    assert phys_loc['region']['startLine'] == expected_line_num
    assert phys_loc['region']['startColumn'] == 11
    assert phys_loc['region']['endColumn'] == 16
    assert phys_loc['contextRegion']['startLine'] == expected_line_num
    assert phys_loc['contextRegion']['snippet']['text'] \
        == '#include <foo.h>\n'

    assert len(results[0]['relatedLocations']) == 1
    note = results[0]['relatedLocations'][0]
    phys_loc = note['physicalLocation']
    assert phys_loc['artifactLocation']['uri'].endswith('test-error-with-note.c')
    assert phys_loc['region']['startLine'] == expected_line_num
    assert phys_loc['region']['startColumn'] == 11
    assert phys_loc['region']['endColumn'] == 16
    assert phys_loc['contextRegion']['startLine'] == expected_line_num
    assert phys_loc['contextRegion']['snippet']['text'] \
        == '#include <foo.h>\n'
    assert note['message']['text'] == 'have you looked behind the couch?'
