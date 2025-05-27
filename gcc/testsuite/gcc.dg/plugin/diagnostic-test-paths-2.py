# Verify that execution paths work in HTML output.

from htmltest import *

import pytest

@pytest.fixture(scope='function', autouse=True)
def html_tree():
    return html_tree_from_env()

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

    event_ranges = diag.find('xhtml:div', ns)
    assert_class(event_ranges, 'event-ranges')

    frame_margin = event_ranges.find('xhtml:table', ns)
    assert_class(frame_margin, 'stack-frame-with-margin')

    tr = frame_margin.find('xhtml:tr', ns)
    assert tr is not None
    tds = tr.findall('xhtml:td', ns)
    assert len(tds) == 2

    assert_class(tds[0], 'interprocmargin')

    test_frame = tds[1]
    assert_frame(test_frame, 'make_a_list_of_random_ints_badly')
    assert_event_range_with_margin(test_frame[1])
