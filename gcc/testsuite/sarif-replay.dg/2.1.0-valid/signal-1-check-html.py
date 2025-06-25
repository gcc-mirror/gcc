from htmltest import *

import pytest

@pytest.fixture(scope='function', autouse=True)
def html_tree():
    return html_tree_from_env()

def test_generated_html(html_tree):
    root = html_tree.getroot ()
    assert root.tag == make_tag('html')

    head = root.find('xhtml:head', ns)
    assert head is not None

    title = head.find('xhtml:title', ns)
    assert title.text == '../../src/gcc/testsuite/gcc.dg/analyzer/signal-1.c'

    diag = get_diag_by_index(html_tree, 0)

    msg = get_message_within_diag(diag)
    assert msg is not None
    
    assert_tag(msg[0], 'strong')
    assert msg[0].text == 'warning: '
    assert msg[0].tail == " call to ‘fprintf’ from within signal handler "
