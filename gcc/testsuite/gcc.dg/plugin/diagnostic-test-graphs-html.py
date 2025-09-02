# Verify that metadata works in HTML output.

from htmltest import *

import pytest

@pytest.fixture(scope='function', autouse=True)
def html_tree():
    return html_tree_from_env()

def test_result_graph(html_tree):
    root = html_tree.getroot ()
    assert root.tag == make_tag('html')

    body = root.find('xhtml:body', ns)
    assert body is not None

    diag_list = body.find('xhtml:div', ns)
    assert diag_list is not None
    assert diag_list.attrib['class'] == 'gcc-diagnostic-list'

    diag = diag_list.find('xhtml:div', ns)
    assert diag is not None

    message = diag.find("./xhtml:div[@class='gcc-message']", ns)
    assert message.attrib['id'] == 'gcc-diag-0-message'

    assert message[0].tag == make_tag('strong')
    assert message[0].tail == ' this is a placeholder error, with graphs'

    graph = diag.find("./xhtml:div[@class='gcc-directed-graph']", ns)
    assert graph is not None

    header = graph.find("./xhtml:h2", ns)
    assert header.text == 'foo'

def test_run_graph(html_tree):
    root = html_tree.getroot ()
    assert root.tag == make_tag('html')

    body = root.find('xhtml:body', ns)
    assert body is not None

    graph = body.find("./xhtml:div[@class='gcc-directed-graph']", ns)
    assert graph is not None

    header = graph.find("./xhtml:h2", ns)
    assert header.text == 'Optimization Passes'
