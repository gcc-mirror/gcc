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

def test_execution_unsuccessful(sarif):
    runs = sarif['runs']
    run = runs[0]

    invocations = run['invocations']
    assert len(invocations) == 1
    invocation = invocations[0]

    # We expect the 'error' to make executionSuccessful be false
    assert invocation['executionSuccessful'] == False

def test_error(sarif):
    runs = sarif['runs']
    run = runs[0]
    results = run['results']

    # We expect a single error
    #
    # . The textual form of the diagnostic would look like this:
    # . PATH/diagnostic-format-sarif-1.F90:4:2:
    # .
    # .    4 | #error message
    # .      |  1~~~~
    # . Error: #error message
    assert len(results) == 1

    result = results[0]
    assert result['level'] == 'error'
    assert result['message']['text'] == "#error message"
    locations = result['locations']
    assert len(locations) == 1

    location = locations[0]
    assert get_location_artifact_uri(location).endswith('diagnostic-format-sarif-1.F90')
    assert get_location_snippet_text(location) == '#error message\n'
    assert get_location_physical_region(location)['startLine'] == 4
    assert get_location_physical_region(location)['startColumn'] == 2
    assert get_location_physical_region(location)['endColumn'] == 7
