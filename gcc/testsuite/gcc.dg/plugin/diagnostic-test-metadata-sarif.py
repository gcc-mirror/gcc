# We expect a warning with this textual form:
#
# . PATH/diagnostic-test-metadata-sarif.c: In function 'test_cwe':
# . PATH/diagnostic-test-metadata-sarif.c:8:3: warning: never use 'gets' [CWE-242] [STR34-C]

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

def test_plugin_metadata(sarif):
    runs = sarif['runs']
    assert len(runs) == 1

    run = runs[0]
    tool = run['tool']

    # We expect an extension, for the plugin.
    extensions = tool['extensions']
    assert len(extensions) == 1

    extension = extensions[0]
    assert extension['name'] == 'diagnostic_plugin_test_metadata'
    assert 'diagnostic_plugin_test_metadata' in extension['fullName']

    # TODO: ideally there would be a rule for [STR34-C] within
    # the extension for the plugin with
    #   'id': 'STR34-C',
    #   'helpUri': 'https://example.com/'

def test_result(sarif):
    runs = sarif['runs']
    run = runs[0]
    results = run['results']

    assert len(results) == 1
    
    result = results[0]
    assert result['level'] == 'warning'
    assert result['message']['text'] == "never use 'gets'"

    assert len(result['taxa']) == 1
    taxon = result['taxa'][0]
    assert taxon['id'] == '242'
    assert taxon['toolComponent']['name'] == 'cwe'
