from sarif import *

import pytest

@pytest.fixture(scope='function', autouse=True)
def sarif():
    return sarif_from_env()

expected_line_num = 8

def test_sarif_output(sarif):
    schema = sarif['$schema']
    assert schema == 'https://docs.oasis-open.org/sarif/sarif/v2.1.0/errata01/os/schemas/sarif-schema-2.1.0.json'

    version = sarif['version']
    assert version == '2.1.0'

    runs = sarif['runs']
    run = runs[0]

    tool = run['tool']
    assert tool['driver']['name'] == 'test-write-sarif-to-file.c.exe'

    invocations = run['invocations']
    assert len(invocations) == 1
    assert 'workingDirectory' in invocations[0]
    assert 'startTimeUtc' in invocations[0]
    assert invocations[0]['executionSuccessful'] == False
    assert invocations[0]['toolExecutionNotifications'] == []
    assert 'endTimeUtc' in invocations[0]

    artifacts = run['artifacts']
    assert len(artifacts) == 1
    assert artifacts[0]['location']['uri'] \
        .endswith('test-write-sarif-to-file.c')
    assert artifacts[0]['sourceLanguage'] == 'c'
    assert 'PRINT' in artifacts[0]['contents']['text']
    assert artifacts[0]['roles'] == ["analysisTarget"]

    results = run['results']
    assert len(results) == 1
    assert results[0]['ruleId'] == 'error'
    assert results[0]['level'] == 'error'
    assert results[0]['message']['text'] == "can't find 'foo'"
    assert len(results[0]['locations']) == 1
    location = results[0]['locations'][0]
    phys_loc = location['physicalLocation']
    assert phys_loc['artifactLocation']['uri'] \
        .endswith('test-write-sarif-to-file.c')
    assert phys_loc['region']['startLine'] == expected_line_num
    assert phys_loc['region']['startColumn'] == 8
    assert phys_loc['region']['endColumn'] == 20
    assert phys_loc['contextRegion']['startLine'] == expected_line_num
    assert phys_loc['contextRegion']['snippet']['text'] \
        == 'PRINT "hello world!";\n'
