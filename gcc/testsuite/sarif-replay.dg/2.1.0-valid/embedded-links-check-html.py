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

    # Get "warning: 004: This is a link."
    diag = get_diag_by_index(html_tree, 3)

    msg = get_message_within_diag(diag)
    assert msg is not None
    
    assert_tag(msg[0], 'strong')
    assert msg[0].text == 'warning: '
    assert msg[0].tail == ' 004: This is a '
    assert_tag(msg[1], 'a')
    assert msg[1].text == 'link'
    assert msg[1].get('href') == 'http://www.example.com'
    assert msg[1].tail == '.'
