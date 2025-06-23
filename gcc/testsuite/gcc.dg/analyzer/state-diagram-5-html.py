# Smoketest of HTML state diagram output

from htmltest import *

import pytest

@pytest.fixture(scope='function', autouse=True)
def html_tree():
    return html_tree_from_env()

def test_state_diagram(html_tree):
    diag = get_diag_by_index(html_tree, 0)
    assert diag is not None

    path = diag.find('xhtml:div[@id="execution-path"]', ns)
    assert path is not None

    event_label = path.find('.//xhtml:span[@id="gcc-diag-0-event-0"]', ns)
    assert event_label is not None
    assert event_label.get('class') == 'event'

    assert event_label.text == '(1) here'

    state_diagram = event_label.find('xhtml:div[@class="state-diagram"]', ns)
    assert state_diagram is not None
    assert state_diagram.get('id') == 'gcc-diag-0-event-0-state-diagram'

    svg = state_diagram.find('.//svg:svg', ns)
    assert svg is not None
