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

def test_result_graph(sarif):
    runs = sarif['runs']
    run = runs[0]
    results = run['results']

    assert len(results) == 1
    
    result = results[0]
    assert result['level'] == 'error'
    assert result['message']['text'] == "this is a placeholder error, with graphs"

    assert len(result['graphs']) == 2

    assert result['graphs'][0]['description']['text'] == 'foo'

    assert len(result['graphs'][0]['nodes']) == 2
    assert result['graphs'][0]['nodes'][0]['id'] == 'a'
    assert result['graphs'][0]['nodes'][1]['id'] == 'b'
    assert result['graphs'][0]['nodes'][1]['properties']['/placeholder-prefix/color'] == 'red'
    assert len(result['graphs'][0]['nodes'][1]['children']) == 1
    assert result['graphs'][0]['nodes'][1]['children'][0]['id'] == 'c'
    assert result['graphs'][0]['nodes'][1]['children'][0]['label']['text'] == 'I am a node label'

    assert len(result['graphs'][0]['edges']) == 1
    result['graphs'][0]['edges'][0]['id'] == 'my-edge'
    assert result['graphs'][0]['edges'][0]['label']['text'] == 'I am an edge label'
    assert result['graphs'][0]['edges'][0]['sourceNodeId'] == 'a'
    assert result['graphs'][0]['edges'][0]['targetNodeId'] == 'c'
    
    assert result['graphs'][1]['description']['text'] == 'bar'

def test_run_graph(sarif):
    runs = sarif['runs']
    run = runs[0]

    assert len(run['graphs']) == 1

    assert run['graphs'][0]['description']['text'] == 'Optimization Passes'
    assert run['graphs'][0]['nodes'][0]['id'] == 'all_lowering_passes'
    assert run['graphs'][0]['edges'][0]['id'] == 'edge0'
