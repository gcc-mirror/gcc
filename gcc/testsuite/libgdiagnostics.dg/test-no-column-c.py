from sarif import *

import pytest

@pytest.fixture(scope='function', autouse=True)
def sarif():
    return sarif_from_env()

expected_line_num = 16

def test_sarif_output(sarif):
    schema = sarif['$schema']
    assert schema == 'https://docs.oasis-open.org/sarif/sarif/v2.1.0/errata01/os/schemas/sarif-schema-2.1.0.json'

    version = sarif['version']
    assert version == '2.1.0'

    runs = sarif['runs']
    run = runs[0]

    tool = run['tool']
    assert tool['driver']['name'] == 'test-no-column.c.exe'

    results = run['results']
    assert len(results) == 1
    location = results[0]['locations'][0]
    phys_loc = location['physicalLocation']
    assert phys_loc['artifactLocation']['uri'].endswith('test-no-column.c')
    assert phys_loc['region']['startLine'] == expected_line_num
    # We should have no column properties:
    assert 'startColumn' not in phys_loc['region']
    assert 'endColumn' not in phys_loc['region']
    assert phys_loc['contextRegion']['startLine'] == expected_line_num
    assert phys_loc['contextRegion']['snippet']['text'] \
        == '#include <foo.h>\n'
