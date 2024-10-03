# We expect a warning with this textual form:
#
# | In file included from PATH/crash-test-ice-header-sarif-2.2.c:5:
# | PATH/crash-test-ice-in-header-sarif-2.2.c: In function 'test_inject_ice':
# | PATH/crash-test-ice-in-header.h:3:22: internal compiler error: I'm sorry Dave, I'm afraid I can't do that
# |     3 | #define INJECT_ICE() inject_ice ()
# |       |                      ^~~~~~~~~~~~~
# | PATH/crash-test-ice-in-header-sarif-2.2.c:9:3: note: in expansion of macro 'INJECT_ICE'
# |     9 |   INJECT_ICE (); /* { dg-ice "" } */
# |       |   ^~~~~~~~~~

from sarif import *

import pytest

@pytest.fixture(scope='function', autouse=True)
def sarif():
    return sarif_from_env()

def test_basics(sarif):
    # We expect SARIF 2.2
    schema = sarif['$schema']
    assert schema == "https://raw.githubusercontent.com/oasis-tcs/sarif-spec/refs/tags/2.2-prerelease-2024-08-08/sarif-2.2/schema/sarif-2-2.schema.json"

    version = sarif['version']
    assert version == "2.2"

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

    # In SARIF 2.2 onwards we should be able to capture the include path
    # as a related location within the notification
    assert len(loc0['relationships']) == 1
    assert loc0['relationships'][0]['kinds'] == ['isIncludedBy']

    assert len(notification['relatedLocations']) == 1
    rel_loc = notification['relatedLocations'][0]

    assert get_location_artifact_uri(rel_loc).endswith('crash-test-ice-in-header-sarif-2.2.c')
    assert '#include "crash-test-ice-in-header.h"' in get_location_snippet_text(rel_loc)
    assert len(rel_loc['relationships']) == 1
    assert rel_loc['relationships'][0]['kinds'] == ['includes']
