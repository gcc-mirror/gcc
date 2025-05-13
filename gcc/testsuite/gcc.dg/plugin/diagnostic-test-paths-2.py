# Verify that execution paths work in HTML output.

from htmltest import *

import pytest

@pytest.fixture(scope='function', autouse=True)
def html_tree():
    return html_tree_from_env()

XHTML = 'http://www.w3.org/1999/xhtml'
ns = {'xhtml': XHTML}

def make_tag(local_name):
    return f'{{{XHTML}}}' + local_name

def test_paths(html_tree):
    root = html_tree.getroot ()
    assert root.tag == make_tag('html')

    body = root.find('xhtml:body', ns)
    assert body is not None

    diag_list = body.find('xhtml:div', ns)
    assert diag_list is not None
    assert diag_list.attrib['class'] == 'gcc-diagnostic-list'

    diag = diag_list.find('xhtml:div', ns)
    assert diag is not None
    assert diag.attrib['class'] == 'gcc-diagnostic'

    pre = diag.findall('xhtml:pre', ns)
    assert pre[0].attrib['class'] == 'gcc-annotated-source'
    assert pre[1].attrib['class'] == 'gcc-execution-path'
    assert pre[1].text.startswith("  'make_a_list_of_random_ints_badly': events 1-3")
