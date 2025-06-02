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

    assert_tag(msg[0], 'span')
    assert_class(msg[0], 'gcc-quoted-text')
    assert_highlighted_text(msg[0][0], 'highlight-a', '%i')

    assert_tag(msg[1], 'span')
    assert_class(msg[1], 'gcc-quoted-text')
    assert_highlighted_text(msg[1][0], 'highlight-a', 'int')

    assert_tag(msg[2], 'span')
    assert_class(msg[2], 'gcc-quoted-text')
    assert_highlighted_text(msg[2][0], 'highlight-b', 'const char *')

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

# For reference, here's the generated HTML:
"""
        <span class="gcc-message" id="gcc-diag-0-message">format &apos;<span class="gcc-quoted-text"><span class="high
light-a">%i</span></span>&apos; expects argument of type &apos;<span class="gcc-quoted-text"><span class="highlight-a"
>int</span></span>&apos;, but argument 2 has type &apos;<span class="gcc-quoted-text"><span class="highlight-b">const 
char *</span></span>&apos;</span>
         
        <span class="gcc-option">[<a href="https://gcc.gnu.org/onlinedocs/gcc/Warning-Options.html#index-Wformat">-Wfo
rmat=</a>]</span>
        <table class="locus">
          <tbody class="line-span">
            <tr><td class="left-margin"> </td><td class="source">  printf(&quot;hello <span class="highlight-a">%i</span>&quot;, <span class="highlight-b">msg</span>);  /* { dg-warning &quot;format &apos;%i&apos; expects argument of type &apos;int&apos;, but argument 2 has type &apos;const char \\*&apos; &quot; } */</td></tr>
            <tr><td class="left-margin"> </td><td class="annotation">                <span class="highlight-a">~^</spa
n>   <span class="highlight-b">~~~</span></td></tr>
            <tr><td class="left-margin"> </td><td class="annotation">                 <span class="highlight-a">|</spa
n>   <span class="highlight-b">|</span></td></tr>
            <tr><td class="left-margin"> </td><td class="annotation">                 <span class="highlight-a">int</s
pan> <span class="highlight-b">const char *</span></td></tr>
            <tr><td class="left-margin"> </td><td class="annotation">                %s</td></tr>
          </tbody>
        </table>
"""
