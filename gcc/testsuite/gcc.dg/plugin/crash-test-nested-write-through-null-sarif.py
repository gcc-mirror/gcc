from sarif import *

import pytest
import re

@pytest.fixture(scope='function', autouse=True)
def sarif():
    return sarif_from_env()

def test_execution_failed(sarif):
    runs = sarif['runs']
    run = runs[0]

    invocations = run['invocations']
    assert len(invocations) == 1
    invocation = invocations[0]

    assert invocation['executionSuccessful'] == False

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

    assert re.match("Segmentation [Ff]ault", notification['message']['text'])
    assert notification['level'] == 'error'

    loc0 = notification['locations'][0]
    assert get_location_artifact_uri(loc0).endswith('crash-test-nested-write-through-null.c')
    assert 'inject_write_through_null ();' in get_location_snippet_text(loc0)

    # We may have backtrace information
    if 'properties' in notification:
        backtrace = notification['properties']['gcc/backtrace']
        assert 'frames' in backtrace
        # Ideally we should have a frame for pass_crash_test::execute(function*)
        # but we can't rely on this.
