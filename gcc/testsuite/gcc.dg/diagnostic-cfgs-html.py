from htmltest import *

import pytest

@pytest.fixture(scope='function', autouse=True)
def html_tree():
    return html_tree_from_env()

def test_results(html_tree):
    root = html_tree.getroot ()
    assert root.tag == make_tag('html')

    head = root.find('xhtml:head', ns)
    assert head is not None

    body = root.find('xhtml:body', ns)
    assert body is not None

    logical_loc = body.find("./xhtml:div[@class='gcc-logical-location']", ns)
    assert len(logical_loc)
    logical_loc.find('xhtml:svg', ns)
