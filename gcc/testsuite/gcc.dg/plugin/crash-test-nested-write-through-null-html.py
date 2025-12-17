from htmltest import *

import pytest
import re

@pytest.fixture(scope='function', autouse=True)
def html_tree():
    return html_tree_from_env()

def test_results(html_tree):
    root = html_tree.getroot ()
    assert root.tag == make_tag('html')

    head = root.find('xhtml:head', ns)
    assert head is not None

    body = root.find('xhtml:body', ns)
    assert body is not None

    diag_list = body.find("./xhtml:div[@class='gcc-diagnostic-list']", ns)
    assert len(diag_list)

    # Currently the ICE appears nested within the parent error
    diag = diag_list.find('xhtml:div', ns)
    assert diag is not None
    assert diag.attrib['class'] == 'alert alert-danger'

    icon = diag.find('xhtml:span', ns)
    assert icon.attrib['class'] == 'pficon pficon-error-circle-o'

    # The message line for the parent error:
    message = diag.find("./xhtml:div[@class='gcc-message']", ns)
    assert message is not None
    assert message[0].tag == make_tag('strong')
    assert message[0].text == 'error: '
    assert message[0].tail == " placeholder"

    # The ICE
    ice = diag.find('.//xhtml:div[@class="gcc-diagnostic"]', ns)
    assert ice is not None
    ice_msg = ice.find("./xhtml:div[@class='gcc-message']", ns)
    assert ice_msg is not None
    assert re.match("Segmentation [Ff]ault", ice_msg.text)
