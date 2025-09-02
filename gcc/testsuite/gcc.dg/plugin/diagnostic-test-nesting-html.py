# Verify that nesting works in HTML output.

from htmltest import *

import pytest

@pytest.fixture(scope='function', autouse=True)
def html_tree():
    return html_tree_from_env()

def test_nesting(html_tree):
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
    assert message[0].tail == ' top-level error'

    # We expect 12 messages, with the given IDs and text:
    for i in range(12):
        child = diag.find(".//xhtml:div[@id='gcc-diag-%i']" % (i + 1),
                          ns)
        assert child is not None

        message = child.find("./xhtml:div[@class='gcc-message']", ns)
        assert message.attrib['id'] == 'gcc-diag-%i-message' % (i + 1)

        if i % 4 == 0:
            assert message.text == 'child %i' % (i / 4)
        else:
            assert message.text == 'grandchild %i %i' % ((i / 4), (i % 4) - 1)

    # We expect the messages to be organized into nested <ul> with
    # "nesting-level" set, all below a <ul>
    child_ul = diag.find("./xhtml:ul[@nesting-level='1']", ns)
    assert child_ul is not None
    msg_id = 1
    for i in range(3):
        child_li = child_ul.find("./xhtml:li[@nesting-level='1'][%i]" % (i + 1), ns)
        assert child_li is not None
        child = child_li.find("./xhtml:div[@id='gcc-diag-%i']" % msg_id, ns)
        assert child is not None
        message = child.find("./xhtml:div[@class='gcc-message']", ns)
        assert message.attrib['id'] == 'gcc-diag-%i-message' % msg_id
        assert message.text == 'child %i' % i
        msg_id += 1
        grandchild_ul = child_ul.find("./xhtml:ul[@nesting-level='2'][%i]" % (i + 1), ns)
        assert grandchild_ul is not None
        for j in range(3):
            grandchild_li = grandchild_ul.find("./xhtml:li[@nesting-level='2'][%i]" % (j + 1), ns)
            assert grandchild_li is not None
            grandchild = grandchild_li.find("./xhtml:div[@id='gcc-diag-%i']" % msg_id, ns)
            assert grandchild is not None
            message = grandchild.find("./xhtml:div[@class='gcc-message']", ns)
            assert message.attrib['id'] == 'gcc-diag-%i-message' % msg_id
            assert message.text == 'grandchild %i %i' % (i, j)
            msg_id += 1
