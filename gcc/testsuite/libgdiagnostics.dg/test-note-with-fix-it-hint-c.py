from sarif import *

import pytest

@pytest.fixture(scope='function', autouse=True)
def sarif():
    return sarif_from_env()

expected_line_num = 21

def test_sarif_output_with_fixes(sarif):
    schema = sarif['$schema']
    assert schema == 'https://docs.oasis-open.org/sarif/sarif/v2.1.0/errata01/os/schemas/sarif-schema-2.1.0.json'

    version = sarif['version']
    assert version == '2.1.0'

    runs = sarif['runs']
    run = runs[0]

    tool = run['tool']
    assert tool['driver']['name'] == 'test-note-with-fix-it-hint.c.exe'

    results = run['results']
    assert len(results) == 1
    result = results[0]
    assert result['ruleId'] == 'error'
    assert result['level'] == 'error'
    assert result['message']['text'] == "unknown field 'colour'"
    assert len(result['locations']) == 1
    location = result['locations'][0]
    phys_loc = location['physicalLocation']
    assert phys_loc['artifactLocation']['uri'].endswith('test-note-with-fix-it-hint.c')
    assert phys_loc['region']['startLine'] == expected_line_num
    assert phys_loc['region']['startColumn'] == 13
    assert phys_loc['region']['endColumn'] == 19
    assert phys_loc['contextRegion']['startLine'] == expected_line_num
    assert phys_loc['contextRegion']['snippet']['text'] \
        == '  return p->colour;\n'

    assert len(result['relatedLocations']) == 1
    note = result['relatedLocations'][0]
    phys_loc = note['physicalLocation']
    assert phys_loc['artifactLocation']['uri'].endswith('test-note-with-fix-it-hint.c')
    assert phys_loc['region']['startLine'] == expected_line_num
    assert phys_loc['region']['startColumn'] == 13
    assert phys_loc['region']['endColumn'] == 19
    assert phys_loc['contextRegion']['startLine'] == expected_line_num
    assert phys_loc['contextRegion']['snippet']['text'] \
        == '  return p->colour;\n'
    assert note['message']['text'] == "did you mean 'color'"

    # TODO: we don't yet capture fix-it hints on notes (PR other/116164)
    assert 'fixes' not in result
