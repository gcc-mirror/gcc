# Verify that metadata works in HTML output.

from htmltest import *

import pytest

@pytest.fixture(scope='function', autouse=True)
def html_tree():
    return html_tree_from_env()

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
    assert diag.attrib['class'] == 'alert alert-warning'

    icon = diag.find('xhtml:span', ns)
    assert icon.attrib['class'] == 'pficon pficon-warning-triangle-o'

    message = diag.find("./xhtml:div[@class='gcc-message']", ns)
    assert message.attrib['id'] == 'gcc-diag-0-message'

    assert message[0].tag == make_tag('strong')
    assert message[0].text == 'warning: '
    assert message[0].tail == " never use '"

    assert message[1].tag == make_tag('span')
    assert message[1].attrib['class'] == 'gcc-quoted-text'
    assert message[1].text == 'gets'
    assert message[1].tail == "' "
    
    metadata = message[2]
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

    src = diag.find('xhtml:table', ns)
    assert src.attrib['class'] == 'locus'

    tbody = src.find('xhtml:tbody', ns)
    assert tbody.attrib['class'] == 'line-span'

    rows = tbody.findall('xhtml:tr', ns)

    quoted_src_tr = rows[0]
    assert_quoted_line(quoted_src_tr,
                       '   10', '  gets (buf);')
    
    annotation_tr = rows[1]
    assert_annotation_line(annotation_tr,
                           '  ^~~~~~~~~~')
