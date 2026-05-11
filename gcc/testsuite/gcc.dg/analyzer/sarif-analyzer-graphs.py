"""
Smoketest for the "analyzer-graphs=yes" option to the sarif
diagnostic sink.
"""

from sarif import *

import pytest

@pytest.fixture(scope='function', autouse=True)
def sarif():
    return sarif_from_env()

def test_supergraph(sarif):
    runs = sarif['runs']
    graphs = runs[0]['graphs']

    sg = graphs[0]
    assert sg['properties']['gcc/digraphs/graph/kind'] == 'supergraph'
    assert sg['description']['text'] == ('"supergraph" used by \'-fanalyzer\' internally to '
                                         "represent the user's code")
    assert len(sg['nodes']) > 0
    assert len(sg['edges']) > 0

def test_exploded_graph(sarif):
    runs = sarif['runs']
    graphs = runs[0]['graphs']

    eg = graphs[1]
    assert eg['description']['text'] == ('"exploded graph" used by \'-fanalyzer\' internally '
                                         'to represent interprocedural control flow in the '
                                         "user's code along with possible states at each "
                                         'program point')
    assert eg['properties']['gcc/digraphs/graph/kind'] == 'exploded_graph'
    assert len(eg['nodes']) > 0
    assert len(eg['edges']) > 0

    dm = eg['properties']['gcc/analyzer/diagnostic_manager']
    assert dm is not None
    assert len(dm['diagnostics']) == 1
    diag = dm['diagnostics'][0]
    assert diag['pending_diagnostic']['kind'] == 'double_free'

    assert eg['properties']['gcc/analyzer/exploded_graph/worklist']['scc']

    ext_state = eg['properties']['gcc/analyzer/extrinsic_state']
    assert ext_state is not None
    assert 'checkers' in ext_state

    kfm = eg['properties']['gcc/analyzer/known_functions']
    assert kfm is not None
    assert 'realloc' in kfm['root_ns']
    assert 'strlen' in kfm['std_ns']
