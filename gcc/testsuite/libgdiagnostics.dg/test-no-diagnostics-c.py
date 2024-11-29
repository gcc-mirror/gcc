from sarif import *

import pytest

@pytest.fixture(scope='function', autouse=True)
def sarif():
    return sarif_from_env()

expected_file_name = 'test-no-diagnostics.c'

def test_sarif_output(sarif):
    schema = sarif['$schema']
    assert schema == 'https://docs.oasis-open.org/sarif/sarif/v2.1.0/errata01/os/schemas/sarif-schema-2.1.0.json'

    version = sarif['version']
    assert version == '2.1.0'

    runs = sarif['runs']
    run = runs[0]

    tool = run['tool']
    assert tool['driver']['name'] == expected_file_name + '.exe'

    invocations = run['invocations']
    assert len(invocations) == 1
    assert 'workingDirectory' in invocations[0]
    assert 'startTimeUtc' in invocations[0]
    assert invocations[0]['executionSuccessful'] == True
    assert invocations[0]['toolExecutionNotifications'] == []
    assert 'endTimeUtc' in invocations[0]

    artifacts = run['artifacts']
    assert len(artifacts) == 1
    assert artifacts[0]['location']['uri'].endswith(expected_file_name)
    assert artifacts[0]['sourceLanguage'] == 'c'
    # We don't bother capturing the contents if there are
    # no diagnostics to display
    assert 'contents' not in artifacts[0]
    assert artifacts[0]['roles'] == ["analysisTarget"]

    results = run['results']
    assert len(results) == 0
