from sarif import *

import pytest

@pytest.fixture(scope='function', autouse=True)
def sarif():
    return sarif_from_env()

def test_messages_have_embedded_urls(sarif):
    runs = sarif['runs']
    run = runs[0]
    results = run['results']

    # We expect a single warning with a secondary location.
    #
    # The textual form of the diagnostic would look like this:
    #  . PATH/bad-pragma.c:7:32: warning: unknown option after '#pragma GCC diagnostic' kind [-Wpragmas]
    #  .     7 | #pragma GCC diagnostic ignored "-Wmisleading-indenttion"
    #  .       |                                ^~~~~~~~~~~~~~~~~~~~~~~~~
    #  . PATH/bad-pragma.c:7:32: note: did you mean '-Wmisleading-indentation'?
    assert len(results) == 1
    
    result = results[0]
    assert result['ruleId'] == '-Wpragmas'
    assert result['level'] == 'warning'
    assert result['message']['text'] \
        == "unknown option after '[#pragma GCC diagnostic](https://gcc.gnu.org/onlinedocs/gcc/Diagnostic-Pragmas.html)' kind"
    # Note how we expect an embedded link in the above for the docs for #pragma GCC diagnostic
    
    # We expect one related location, for the note.
    relatedLocations = result['relatedLocations']
    assert len(relatedLocations) == 1

    rel_loc = relatedLocations[0]
    assert rel_loc['message']['text'] \
        == "did you mean '[-Wmisleading-indentation](https://gcc.gnu.org/onlinedocs/gcc/Warning-Options.html#index-Wmisleading-indentation)'?"
    # Again, we expect an embedded link in the above, this time to the
    # docs for the suggested option
