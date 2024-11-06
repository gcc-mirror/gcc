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

def test_error_location(sarif):
    runs = sarif['runs']
    run = runs[0]
    results = run['results']

    # We expect a single error with annotations.
    #
    # The textual form of the diagnostic looks like this:
    # . PATH/bad-binary-ops.c: In function 'test_4':
    # . PATH/bad-binary-ops.c:64:23: error: invalid operands to binary + (have 'S' {aka 'struct s'} and 'T' {aka 'struct t'})
    # .    return callee_4a () + callee_4b (); /* { dg-error "invalid operands to binary \+" } */
    # .           ~~~~~~~~~~~~ ^ ~~~~~~~~~~~~
    # .           |              |
    # .           |              T {aka struct t}
    # .           S {aka struct s}
    assert len(results) == 1

    result = results[0]
    assert result['level'] == 'error'

    assert result['message']['text'] \
        == "invalid operands to binary + (have 'S' {aka 'struct s'} and 'T' {aka 'struct t'})"
    locations = result['locations']
    assert len(locations) == 1

    location = locations[0]
    assert get_location_artifact_uri(location).endswith('bad-binary-op.c')
    assert get_location_snippet_text(location) \
        == "  return callee_4a () + callee_4b (); /* { dg-error \"invalid operands to binary \\+\" } */\n"
    EXPECTED_LINE = 14
    assert get_location_physical_region(location)['startLine'] == EXPECTED_LINE
    assert get_location_physical_region(location)['startColumn'] == 23
    assert get_location_physical_region(location)['endColumn'] == 24

    annotations = location['annotations']
    assert len(annotations) == 2
    assert annotations[0]['startLine'] == EXPECTED_LINE
    assert annotations[0]['startColumn'] == 10
    assert annotations[0]['endColumn'] == 22
    assert annotations[0]['message']['text'] == "S {aka struct s}"
    assert annotations[1]['startLine'] == EXPECTED_LINE
    assert annotations[1]['startColumn'] == 25
    assert annotations[1]['endColumn'] == 37
    assert annotations[1]['message']['text'] == "T {aka struct t}"
