# Verify that basics of HTML output work.
#
# For reference, we expect this textual output:
#
# PATH/missing-semicolon.c: In function ‘missing_semicolon’:
# PATH/missing-semicolon.c:8:12: error: expected ‘;’ before ‘}’ token
#     8 |   return 42 /* { dg-error "expected ';' before '.' token" } */
#       |            ^
#       |            ;
#     9 | }
#       | ~           

from htmltest import *

import pytest

@pytest.fixture(scope='function', autouse=True)
def html_tree():
    return html_tree_from_env()

def test_basics(html_tree):
    root = html_tree.getroot ()
    assert root.tag == make_tag('html')

    head = root.find('xhtml:head', ns)
    assert head is not None

    title = head.find('xhtml:title', ns)
    assert title.text.endswith('gcc/testsuite/gcc.dg/html-output/missing-semicolon.c')

    body = root.find('xhtml:body', ns)
    assert body is not None

    diag_list = body.find('xhtml:div', ns)
    assert diag_list is not None
    assert diag_list.attrib['class'] == 'gcc-diagnostic-list'

    diag = diag_list.find('xhtml:div', ns)
    assert diag is not None
    assert diag.attrib['class'] == 'alert alert-danger'
    assert diag.attrib['id'] == 'gcc-diag-0'

    icon = diag.find('xhtml:span', ns)
    assert icon.attrib['class'] == 'pficon pficon-error-circle-o'

    # The message line:
    message = diag.find("./xhtml:div[@class='gcc-message']", ns)
    assert message is not None
    # <html:div xmlns:html="http://www.w3.org/1999/xhtml" class="gcc-message" id="gcc-diag-0-message"><html:strong>error: </html:strong> expected '<html:span class="gcc-quoted-text">;</html:span>' before '<html:span class="gcc-quoted-text">}</html:span>' token</html:div>
    assert message[0].tag == make_tag('strong')
    assert message[0].text == 'error: '
    assert message[0].tail == " expected '"
    assert message[1].tag == make_tag('span')
    assert message[1].attrib['class'] == 'gcc-quoted-text'
    assert message[1].text == ';'
    assert message[1].tail == "' before '"
    assert message[2].tag == make_tag('span')
    assert message[2].attrib['class'] == 'gcc-quoted-text'
    assert message[2].text == '}'
    assert message[2].tail == "' token"

    # Logical location
    logical_loc = diag.find("./xhtml:div[@id='logical-location']", ns)
    assert logical_loc is not None
    assert len(logical_loc) == 2
    assert logical_loc[0].tag == make_tag('span')
    assert logical_loc[0].text == 'Function '
    assert logical_loc[1].tag == make_tag('span')
    assert logical_loc[1].text == 'missing_semicolon'
    assert logical_loc[1].attrib['class'] == 'gcc-quoted-text'

    # Physical location
    file_ = diag.find("./xhtml:div[@id='file']", ns)
    assert file_ is not None
    assert len(file_) == 2
    assert file_[0].tag == make_tag('span')
    assert file_[0].text == 'File '
    assert file_[1].tag == make_tag('span')
    assert file_[1].text.endswith('gcc/testsuite/gcc.dg/html-output/missing-semicolon.c')

    line = diag.find("./xhtml:div[@id='line']", ns)
    assert line is not None
    assert len(line) == 2
    assert line[0].tag == make_tag('span')
    assert line[0].text == 'Line '
    assert line[1].tag == make_tag('span')
    assert line[1].text == '8'

    column = diag.find("./xhtml:div[@id='column']", ns)
    assert column is not None
    assert len(column) == 2
    assert column[0].tag == make_tag('span')
    assert column[0].text == 'Column '
    assert column[1].tag == make_tag('span')
    assert column[1].text == '12'

    # Suggested fix
    fix = diag.find("./xhtml:div[@id='suggested-fix']", ns)
    label = fix.find('xhtml:label', ns)
    assert label.text == "Suggested fix"
    pre = fix.find('xhtml:pre', ns)
    assert pre is not None
    assert pre.attrib['class'] == 'gcc-generated-patch'
    assert pre.text.startswith('--- ')
