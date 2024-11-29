from sarif import *

import pytest

@pytest.fixture(scope='function', autouse=True)
def sarif():
    return sarif_from_env()

def test_sarif_output_with_fixes(sarif):
    schema = sarif['$schema']
    assert schema == 'https://docs.oasis-open.org/sarif/sarif/v2.1.0/errata01/os/schemas/sarif-schema-2.1.0.json'

    version = sarif['version']
    assert version == '2.1.0'

    runs = sarif['runs']
    run = runs[0]

    tool = run['tool']
    assert tool['driver']['name'] == 'test-fix-it-hint.c.exe'

    results = run['results']
    assert len(results) == 1
    assert results[0]['ruleId'] == 'error'
    assert results[0]['level'] == 'error'
    assert results[0]['message']['text'] == "unknown field 'colour'; did you mean 'color'"
    assert len(results[0]['locations']) == 1
    location = results[0]['locations'][0]
    phys_loc = location['physicalLocation']
    assert phys_loc['artifactLocation']['uri'].endswith('test-fix-it-hint.c')
    assert phys_loc['region']['startLine'] == 19
    assert phys_loc['region']['startColumn'] == 13
    assert phys_loc['region']['endColumn'] == 19
    assert phys_loc['contextRegion']['startLine'] == 19
    assert phys_loc['contextRegion']['snippet']['text'] \
        == '  return p->colour;\n'

    assert len(results[0]['fixes']) == 1
    fix = results[0]['fixes'][0]
    assert len(fix['artifactChanges']) == 1
    change = fix['artifactChanges'][0]
    assert change['artifactLocation']['uri'].endswith('test-fix-it-hint.c')
    assert len(change['replacements']) == 1
    replacement = change['replacements'][0]
    assert replacement['deletedRegion'] == phys_loc['region']
    assert replacement['insertedContent']['text'] == 'color'
