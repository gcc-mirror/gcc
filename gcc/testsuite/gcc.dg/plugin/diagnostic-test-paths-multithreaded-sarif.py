# We expect a warning with this textual form:
#
# . warning: deadlock due to inconsistent lock acquisition order
# .    17 |   acquire_lock_a (); /* { dg-warning "deadlock due to inconsistent lock acquisition order" } */
# .       |   ^~~~~~~~~~~~~~~~~
# . Thread: 'Thread 1'
# .   'foo': event 1
# .     |
# .     |    9 | {
# .     |      | ^
# .     |      | |
# .     |      | (1) entering 'foo'
# .     |
# .     +--> 'foo': event 2
# .            |
# .            |   10 |   acquire_lock_a ();
# .            |      |   ^~~~~~~~~~~~~~~~~
# .            |      |   |
# .            |      |   (2) lock a is now held by thread 1
# .            |
# . 
# . Thread: 'Thread 2'
# .   'bar': event 3
# .     |
# .     |   15 | {
# .     |      | ^
# .     |      | |
# .     |      | (3) entering 'bar'
# .     |
# .     +--> 'bar': event 4
# .            |
# .            |   16 |   acquire_lock_b ();
# .            |      |   ^~~~~~~~~~~~~~~~~
# .            |      |   |
# .            |      |   (4) lock b is now held by thread 2
# .            |

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

def test_execution_successful(sarif):
    runs = sarif['runs']
    run = runs[0]

    invocations = run['invocations']
    assert len(invocations) == 1
    invocation = invocations[0]

    # We expect a mere 'warning' to allow executionSuccessful be true
    assert invocation['executionSuccessful'] == True

def test_result(sarif):
    runs = sarif['runs']
    run = runs[0]
    results = run['results']

    assert len(results) == 1
    
    result = results[0]
    assert result['level'] == 'warning'
    assert result['message']['text'] == "deadlock due to inconsistent lock acquisition order"

    code_flows = result['codeFlows']
    assert len(code_flows) == 1

    code_flow = code_flows[0]

    thread_flows = code_flow['threadFlows']
    assert len(thread_flows) == 2

    tf0 = thread_flows[0]
    assert tf0['id'] == 'Thread 1'
    
    tf1 = thread_flows[1]
    assert tf1['id'] == 'Thread 2'

    assert len(tf0['locations']) == 3
    assert tf0['locations'][0]['executionOrder'] == 1
    assert tf0['locations'][0]['location']['message']['text'] \
        == "entering 'foo'"
    assert tf0['locations'][1]['executionOrder'] == 2
    assert tf0['locations'][1]['location']['message']['text'] \
        == "lock a is now held by thread 1"
    assert tf0['locations'][2]['executionOrder'] == 5
    assert tf0['locations'][2]['location']['message']['text'] \
        == "deadlocked due to waiting for lock b in thread 1 (acquired by thread 2 at (4))..."

    assert len(tf1['locations']) == 3
    assert tf1['locations'][0]['executionOrder'] == 3
    assert tf1['locations'][0]['location']['message']['text'] \
        == "entering 'bar'"
    assert tf1['locations'][1]['executionOrder'] == 4
    assert tf1['locations'][1]['location']['message']['text'] \
        == "lock b is now held by thread 2"
    assert tf1['locations'][2]['executionOrder'] == 6
    assert tf1['locations'][2]['location']['message']['text'] \
        == "...whilst waiting for lock a in thread 2 (acquired by thread 1 at (2))"
