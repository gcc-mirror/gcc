from htmltest import *

import pytest

@pytest.fixture(scope='function', autouse=True)
def html_tree():
    return html_tree_from_env()

def assert_highlighted_text(element, expected_highlight, expected_text):
    assert_tag(element, 'span')
    assert_class(element, expected_highlight)
    assert element.text == expected_text

def test_message(html_tree):
    """
    Verify that the quoted text in the message has the correct
    highlight colors.
    """
    diag = get_diag_by_index(html_tree, 0)
    msg = get_message_within_diag(diag)

    assert_tag(msg[0], 'strong')
    assert msg[0].text == 'warning: '
    
    assert_tag(msg[1], 'span')
    assert_class(msg[1], 'gcc-quoted-text')
    assert_highlighted_text(msg[1][0], 'highlight-a', '%i')

    assert_tag(msg[2], 'span')
    assert_class(msg[2], 'gcc-quoted-text')
    assert_highlighted_text(msg[2][0], 'highlight-a', 'int')

    assert_tag(msg[3], 'span')
    assert_class(msg[3], 'gcc-quoted-text')
    assert_highlighted_text(msg[3][0], 'highlight-b', 'const char *')

def test_annotations(html_tree):
    """
    Verify that the labels in the annotations have the correct
    highlight colors.
    """
    diag = get_diag_by_index(html_tree, 0)
    locus = get_locus_within_diag(diag)
    tbody = locus.find('xhtml:tbody', ns)
    assert tbody.attrib['class'] == 'line-span'

    rows = tbody.findall('xhtml:tr', ns)

    # Source row
    row = rows[0]
    tds = row.findall('xhtml:td', ns)
    assert len(tds) == 2
    assert_class(tds[1], 'source')
    assert_highlighted_text(tds[1][0], 'highlight-a', '%i')
    assert_highlighted_text(tds[1][1], 'highlight-b', 'msg')
    
    # Underline row:
    row = rows[1]
    tds = row.findall('xhtml:td', ns)
    assert len(tds) == 2
    assert_class(tds[1], 'annotation')
    assert_highlighted_text(tds[1][0], 'highlight-a', '~^')
    assert_highlighted_text(tds[1][1], 'highlight-b', '~~~')
    
    # vline row:
    row = rows[2]
    tds = row.findall('xhtml:td', ns)
    assert len(tds) == 2
    assert_class(tds[1], 'annotation')
    assert_highlighted_text(tds[1][0], 'highlight-a', '|')
    assert_highlighted_text(tds[1][1], 'highlight-b', '|')
    
    # Label row:
    row = rows[3]
    tds = row.findall('xhtml:td', ns)
    assert len(tds) == 2
    assert_class(tds[1], 'annotation')
    assert_highlighted_text(tds[1][0], 'highlight-a', 'int')
    assert_highlighted_text(tds[1][1], 'highlight-b', 'const char *')
