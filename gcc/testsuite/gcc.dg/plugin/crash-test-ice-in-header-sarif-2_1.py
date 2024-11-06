# We expect a warning with this textual form:
#
# | In file included from PATH/crash-test-ice-header-sarif-2.1.c:5:
# | PATH/crash-test-ice-in-header-sarif-2.1.c: In function 'test_inject_ice':
# | PATH/crash-test-ice-in-header.h:3:22: internal compiler error: I'm sorry Dave, I'm afraid I can't do that
# |     3 | #define INJECT_ICE() inject_ice ()
# |       |                      ^~~~~~~~~~~~~
# | PATH/crash-test-ice-in-header-sarif-2.1.c:9:3: note: in expansion of macro 'INJECT_ICE'
# |     9 |   INJECT_ICE (); /* { dg-ice "" } */
# |       |   ^~~~~~~~~~

from sarif import *

import pytest

@pytest.fixture(scope='function', autouse=True)
def sarif():
    return sarif_from_env()

def test_basics(sarif):
    # We expect SARIF 2.1
    schema = sarif['$schema']
    assert schema == "https://docs.oasis-open.org/sarif/sarif/v2.1.0/errata01/os/schemas/sarif-schema-2.1.0.json"

    version = sarif['version']
    assert version == "2.1.0"

def test_no_result(sarif):
    # We expect no results in the run
    runs = sarif['runs']
    run = runs[0]
    results = run['results']
    assert len(results) == 0

def test_notification(sarif):
    # We expect an execution notification for the ICE in the invocation
    runs = sarif['runs']
    run = runs[0]

    invocations = run['invocations']
    assert len(invocations) == 1
    invocation = invocations[0]

    assert invocation['executionSuccessful'] == False

    notifications = invocation['toolExecutionNotifications']
    assert len(notifications) == 1

    notification = notifications[0]

    assert notification['message']['text'] == "I'm sorry Dave, I'm afraid I can't do that"
    assert notification['level'] == 'error'

    loc0 = notification['locations'][0]
    assert get_location_artifact_uri(loc0).endswith('crash-test-ice-in-header.h')
    assert 'inject_ice ();' in get_location_snippet_text(loc0)

    # We may have backtrace information
    if 'properties' in notification:
        backtrace = notification['properties']['gcc/backtrace']
        assert 'frames' in backtrace
        # Ideally we should have a frame for pass_crash_test::execute(function*)
        # but we can't rely on this.

    # In SARIF 2.1 and earlier we aren't able to capture the include path
    # as a related location within the notification
    assert 'relationships' not in loc0
    assert 'relatedLocations' not in notification
