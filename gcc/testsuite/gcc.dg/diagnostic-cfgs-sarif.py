from sarif import *

import pytest

@pytest.fixture(scope='function', autouse=True)
def sarif():
    return sarif_from_env()

def get_graph_by_description(sarif, desc):
    runs = sarif['runs']
    run = runs[0]
    graphs = run['graphs']
    for g in graphs:
        if g['description']['text'] == desc:
            return g

def test_graphs(sarif):
    runs = sarif['runs']
    run = runs[0]
    graphs = run['graphs']

    # We expect many CFGs, but let's not hardcode the number
    assert len(graphs) > 20
    
def test_ssa(sarif):
    ssa = get_graph_by_description(sarif, 'test: ssa')
    assert ssa["properties"]["gcc/digraphs/graph/kind"] == "cfg"
    assert ssa["properties"]["gcc/cfg/graph/pass_name"] == "ssa"
    assert type(ssa["properties"]["gcc/cfg/graph/pass_number"]) == int
    assert ssa["properties"]["logicalLocation"]["fullyQualifiedName"] == "test"
    nodes = ssa["nodes"]
    assert len(nodes) == 1
    root_node = nodes[0]
    assert root_node['id'] == 'test'
    assert root_node['properties']["gcc/cfg/node/kind"] == "function"

    assert len(root_node['children']) >= 3
    entry = root_node['children'][0]
    assert entry['properties']["gcc/cfg/node/kind"] == "basic_block"
    assert entry['properties']["gcc/cfg/basic_block/kind"] == "entry"
    assert entry['properties']["gcc/cfg/basic_block/index"] == 0
    assert entry['properties']["gcc/cfg/basic_block/gimple/phis"] == []
    assert entry['properties']["gcc/cfg/basic_block/gimple/stmts"] == []

    exit = root_node['children'][1]
    assert exit['properties']["gcc/cfg/node/kind"] == "basic_block"
    assert exit['properties']["gcc/cfg/basic_block/kind"] == "exit"
    assert exit['properties']["gcc/cfg/basic_block/index"] == 1
    assert exit['properties']["gcc/cfg/basic_block/gimple/phis"] == []
    assert exit['properties']["gcc/cfg/basic_block/gimple/stmts"] == []
    
    block = root_node['children'][2]
    assert block['properties']["gcc/cfg/node/kind"] == "basic_block"
    assert block['properties']["gcc/cfg/basic_block/index"] == 2
    assert block['properties']["gcc/cfg/basic_block/gimple/phis"] == []
    cond = block['properties']["gcc/cfg/basic_block/gimple/stmts"][0]
    assert cond.startswith("if (i_")

    return_block = root_node['children'][-1]
    phis = return_block['properties']["gcc/cfg/basic_block/gimple/phis"]
    assert len(phis) == 1
    assert '= PHI' in phis[0]
    
    edges = ssa["edges"]
    assert len(edges) >= 4

def test_expand(sarif):
    ssa = get_graph_by_description(sarif, 'test: expand')
    assert ssa["properties"]["gcc/digraphs/graph/kind"] == "cfg"
    assert ssa["properties"]["logicalLocation"]["fullyQualifiedName"] == "test"
    nodes = ssa["nodes"]
    assert len(nodes) == 1
    root_node = nodes[0]
    assert root_node['id'] == 'test'
    assert root_node['properties']["gcc/cfg/node/kind"] == "function"

    assert len(root_node['children']) >= 3
    entry = root_node['children'][0]
    assert entry['properties']["gcc/cfg/node/kind"] == "basic_block"
    assert entry['properties']["gcc/cfg/basic_block/kind"] == "entry"
    assert entry['properties']["gcc/cfg/basic_block/rtl/insns"] == []

    edges = ssa["edges"]
    assert len(edges) >= 4
