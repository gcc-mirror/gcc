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

    diag_list = body.find("./xhtml:div[@class='gcc-diagnostic-list']", ns)
    assert len(diag_list)

    diag = diag_list.find('xhtml:div[@id="gcc-diag-0"]', ns)
    assert diag is not None
    message = diag.find("./xhtml:div[@class='gcc-message']", ns)
    assert message is not None
    assert message[0].tag == make_tag('strong')
    assert message[0].text == 'fatal error: '
    assert message[0].tail.startswith(' this-does-not-exist.h:')
