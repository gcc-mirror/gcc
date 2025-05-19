# Verify that metadata works in HTML output.

from htmltest import *

import pytest

@pytest.fixture(scope='function', autouse=True)
def html_tree():
    return html_tree_from_env()

XHTML = 'http://www.w3.org/1999/xhtml'
ns = {'xhtml': XHTML}

def make_tag(local_name):
    return f'{{{XHTML}}}' + local_name

def test_metadata(html_tree):
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

    spans = diag.findall('xhtml:span', ns)
    metadata = spans[1]
    assert metadata.attrib['class'] == 'gcc-metadata'
    assert metadata[0].tag == make_tag('span')
    assert metadata[0].attrib['class'] == 'gcc-metadata-item'
    assert metadata[0].text == '['
    assert metadata[0][0].tag == make_tag('a')
    assert metadata[0][0].attrib['href'] == 'https://cwe.mitre.org/data/definitions/242.html'
    assert metadata[0][0].text == 'CWE-242'
    assert metadata[0][0].tail == ']'

    assert metadata[1].tag == make_tag('span')
    assert metadata[1].attrib['class'] == 'gcc-metadata-item'
    assert metadata[1].text == '['
    assert metadata[1][0].tag == make_tag('a')
    assert metadata[1][0].attrib['href'] == 'https://example.com/'
    assert metadata[1][0].text == 'STR34-C'
    assert metadata[1][0].tail == ']'

    src = diag.find('xhtml:pre', ns)
    assert src.attrib['class'] == 'gcc-annotated-source'
    assert src.text == (
        '   gets (buf);\n'
        '   ^~~~~~~~~~\n')

# For reference, here's the generated HTML:
"""
  <body>
    <div class="gcc-diagnostic-list">
      <div class="gcc-diagnostic">
        <span class="gcc-message">never use &apos;<span class="gcc-quoted-text">gets</span>&apos;</span> 
        <span class="gcc-metadata"><span class="gcc-metadata-item">[<a href="https://cwe.mitre.org/data/definitions/242.html">CWE-242</a>]</span><span class="gcc-metadata-item">[<a href="https://example.com/">STR34-C</a>]</span></span>
        ...etc...
      </div>
    </div>
  </body>
"""
