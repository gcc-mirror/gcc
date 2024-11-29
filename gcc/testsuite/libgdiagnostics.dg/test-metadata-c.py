from sarif import *

import pytest

@pytest.fixture(scope='function', autouse=True)
def sarif():
    return sarif_from_env()

def test_sarif_output_metadata(sarif):
    schema = sarif['$schema']
    assert schema == 'https://docs.oasis-open.org/sarif/sarif/v2.1.0/errata01/os/schemas/sarif-schema-2.1.0.json'

    version = sarif['version']
    assert version == '2.1.0'

    runs = sarif['runs']
    run = runs[0]

    tool = run['tool']
    assert tool['driver']['name'] == 'FooChecker'
    assert tool['driver']['fullName'] == 'FooChecker 0.1 (en_US)'
    assert tool['driver']['version'] == '0.1'
    assert tool['driver']['informationUri'] == 'https://www.example.com/0.1/'

    taxonomies = run["taxonomies"]
    assert len(taxonomies) == 1

    cwe = taxonomies[0]
    assert cwe['name'] == 'CWE'
    assert cwe['version'] == '4.7'
    assert cwe['organization'] == 'MITRE'
    assert cwe['shortDescription']['text'] \
        == 'The MITRE Common Weakness Enumeration'
    assert len(cwe['taxa']) == 1
    assert cwe['taxa'][0]['id'] == '242'
    assert cwe['taxa'][0]['helpUri'] \
        == 'https://cwe.mitre.org/data/definitions/242.html'
    
    results = run['results']
    assert len(results) == 1

    result = results[0]
    assert result['ruleId'] == 'warning'
    assert result['level'] == 'warning'
    assert result['message']['text'] == "never use 'gets'"
