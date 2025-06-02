# Verify that diagnostic-show-locus.cc works with HTML output.

from htmltest import *

import pytest

@pytest.fixture(scope='function', autouse=True)
def html_tree():
    return html_tree_from_env()

#def get_tr_within_thead(thead, idx)

def get_ruler_text(thead, idx):
    trs = thead.findall('xhtml:tr', ns)
    tr = trs[idx]
    tds = tr.findall('xhtml:td', ns)
    assert len(tds) == 3
    assert_class(tds[2], 'ruler')
    return tds[2].text

def test_very_wide_line(html_tree):
    diag = get_diag_by_index(html_tree, 2)
    src = get_locus_within_diag(diag)

    # Check ruler
    thead = src.find('xhtml:thead', ns)
    assert_class(thead, 'ruler')
    trs = thead.findall('xhtml:tr', ns)
    assert len(trs) == 3

    assert get_ruler_text(thead, 0) == '       0         0         0         0         0         1         1   '
    assert get_ruler_text(thead, 1) == '       5         6         7         8         9         0         1   '
    assert get_ruler_text(thead, 2) == '34567890123456789012345678901234567890123456789012345678901234567890123'

    # Check quoted source
    tbody = src.find('xhtml:tbody', ns)
    assert_class(tbody, 'line-span')
    trs = tbody.findall('xhtml:tr', ns)
    assert len(trs) == 5
    assert_quoted_line(trs[0], '   43', '                                      float f = foo * bar; /* { dg-warning "95: test" } */')
    assert_annotation_line(trs[1],      '                                                ~~~~^~~~~')
    assert_annotation_line(trs[2],      '                                                    |')
    assert_annotation_line(trs[3],      '                                                    label 0')
    assert_annotation_line(trs[4],      '                                                bar * foo')

def test_fixit_insert(html_tree):
    diag = get_diag_by_index(html_tree, 3)
    msg = get_message_within_diag(diag)
    assert msg.text == 'example of insertion hints'

    src = get_locus_within_diag(diag)

    # Check quoted source
    tbody = src.find('xhtml:tbody', ns)
    assert_class(tbody, 'line-span')
    trs = tbody.findall('xhtml:tr', ns)
    assert len(trs) == 3
    assert_quoted_line(trs[0], '   63', '   int a[2][2] = { 0, 1 , 2, 3 }; /* { dg-warning "insertion hints" } */')
    assert_annotation_line(trs[1],      '                   ^~~~')
    assert_annotation_line(trs[2],      '                   {   }')

def test_fixit_remove(html_tree):
    diag = get_diag_by_index(html_tree, 4)
    msg = get_message_within_diag(diag)
    assert msg.text == 'example of a removal hint'

    src = get_locus_within_diag(diag)

    # Check quoted source
    tbody = src.find('xhtml:tbody', ns)
    assert_class(tbody, 'line-span')
    trs = tbody.findall('xhtml:tr', ns)
    assert len(trs) == 3
    assert_quoted_line(trs[0], '   77', '  int a;; /* { dg-warning "example of a removal hint" } */')
    assert_annotation_line(trs[1],      '        ^')
    assert_annotation_line(trs[2],      '        -')

def test_fixit_replace(html_tree):
    diag = get_diag_by_index(html_tree, 5)
    msg = get_message_within_diag(diag)
    assert msg.text == 'example of a replacement hint'

    src = get_locus_within_diag(diag)

    # Check quoted source
    tbody = src.find('xhtml:tbody', ns)
    assert_class(tbody, 'line-span')
    trs = tbody.findall('xhtml:tr', ns)
    assert len(trs) == 3
    assert_quoted_line(trs[0], '   91', '  gtk_widget_showall (dlg); /* { dg-warning "example of a replacement hint" } */')
    assert_annotation_line(trs[1],      '  ^~~~~~~~~~~~~~~~~~')
    assert_annotation_line(trs[2],      '  gtk_widget_show_all')

def test_fixit_insert_newline(html_tree):
    diag = get_diag_by_index(html_tree, 6)
    msg = get_message_within_diag(diag)
    assert msg.text == 'example of newline insertion hint'

    src = get_locus_within_diag(diag)

    # Check quoted source
    tbody = src.find('xhtml:tbody', ns)
    assert_class(tbody, 'line-span')
    trs = tbody.findall('xhtml:tr', ns)
    assert len(trs) == 4
    assert_quoted_line(trs[0], '  109', '      x = a;')
    assert_annotation_line(trs[1],      '      break;',
                           expected_line_num='  +++',
                           expected_left_margin='+')
    assert_quoted_line(trs[2], '  110', "    case 'b':  /* { dg-warning \"newline insertion\" } */")
    assert_annotation_line(trs[3],      '    ^~~~~~~~')
