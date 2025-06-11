# Verify that interprocedural execution paths work in HTML output.

from htmltest import *

import pytest

@pytest.fixture(scope='function', autouse=True)
def html_tree():
    return html_tree_from_env()

def test_paths(html_tree):
    diag = get_diag_by_index(html_tree, 0)
    src = get_locus_within_diag (diag)

    tbody = src.find('xhtml:tbody', ns)
    assert_class(tbody, 'line-span')

    rows = tbody.findall('xhtml:tr', ns)

    quoted_src_tr = rows[0]
    assert_quoted_line(quoted_src_tr,
                       '   13', '  fprintf(stderr, "LOG: %s", msg); /* { dg-warning "call to \'fprintf\' from within signal handler" } */')
    
    annotation_tr = rows[1]
    assert_annotation_line(annotation_tr,
                           '  ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')

    exec_path = diag.find("./xhtml:div[@id='execution-path']", ns)
    assert exec_path is not None

    label = exec_path.find('xhtml:label', ns)
    assert label.text == 'Execution path with 9 events'
    
    event_ranges = exec_path.find('xhtml:div', ns)
    assert_class(event_ranges, 'event-ranges')

    test_frame_margin = event_ranges.find('xhtml:table', ns)
    assert_class(test_frame_margin, 'stack-frame-with-margin')

    tr = test_frame_margin.find('xhtml:tr', ns)
    assert tr is not None
    tds = tr.findall('xhtml:td', ns)
    assert len(tds) == 2

    assert_class(tds[0], 'interprocmargin')

    test_frame = tds[1]
    assert_frame(test_frame, 'test')
    assert_event_range_with_margin(test_frame[1])
