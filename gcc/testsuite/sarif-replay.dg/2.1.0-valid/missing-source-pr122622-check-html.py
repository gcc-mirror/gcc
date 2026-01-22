from htmltest import *

import pytest

@pytest.fixture(scope='function', autouse=True)
def html_tree():
    return html_tree_from_env()

def test_generated_html(html_tree):
    root = html_tree.getroot ()
    assert root.tag == make_tag('html')

    head = root.find('xhtml:head', ns)
    assert head is not None

    diag = get_diag_by_index(html_tree, 0)

    exec_path = diag.find("./xhtml:div[@id='execution-path']", ns)
    assert exec_path is not None

    label = exec_path.find('xhtml:label', ns)
    assert label.text == 'Execution path with 18 events'

    final_event = exec_path.find(".//xhtml:span[@id='gcc-diag-0-event-17']", ns)
    assert (ET.tostring(final_event, method='html').rstrip()
            == (b'<html:span xmlns:html="http://www.w3.org/1999/xhtml" class="event" id="gcc-diag-0-event-17">'
                b'<html:span class="location">../test/create_cert_test.c:118:3:</html:span>'
                b'<html:span class="event-id"> (18): </html:span>'
                b'<html:span class="event-text">&#8216;ext_str&#8217; leaks here; was allocated at (13)</html:span>'
                b'</html:span>'))
