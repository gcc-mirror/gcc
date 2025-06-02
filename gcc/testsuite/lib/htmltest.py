import os
import xml.etree.ElementTree as ET

def html_tree_from_env():
    # return parsed HTML content as an ET from an HTML_PATH file
    html_filename = os.environ['HTML_PATH']
    html_filename += '.html'
    print('html_filename: %r' % html_filename)
    return ET.parse(html_filename)

XHTML = 'http://www.w3.org/1999/xhtml'
ns = {'xhtml': XHTML}

def make_tag(local_name):
    return f'{{{XHTML}}}' + local_name

def assert_tag(element, expected):
    assert element.tag == make_tag(expected)

def assert_class(element, expected):
    assert element.attrib['class'] == expected

def assert_quoted_line(tr, expected_line_num, expected_src):
    """Verify that tr is a line of quoted source."""
    tds = tr.findall('xhtml:td', ns)
    assert len(tds) == 3
    assert_class(tds[0], 'linenum')
    assert tds[0].text == expected_line_num
    assert_class(tds[1], 'left-margin')
    assert tds[1].text == ' '
    assert_class(tds[2], 'source')
    assert tds[2].text == expected_src

def assert_annotation_line(tr, expected_src,
                           expected_line_num='     ',
                           expected_left_margin=' '):
    """Verify that tr is an annotation line."""
    tds = tr.findall('xhtml:td', ns)
    assert len(tds) == 3
    assert_class(tds[0], 'linenum')
    assert tds[0].text == expected_line_num
    assert_class(tds[1], 'left-margin')
    assert tds[1].text == expected_left_margin
    assert_class(tds[2], 'annotation')
    assert tds[2].text == expected_src

def assert_frame(frame, expected_fnname):
    """
    Assert that frame is of class 'stack-frame'
    and has a child showing the expected fnname.
    """
    assert_class(frame, 'stack-frame')
    funcname = frame[0]
    assert_class(funcname, 'frame-funcname')
    span = funcname[0]
    assert_tag(span, 'span')
    assert span.text == expected_fnname

def assert_event_range_with_margin(element):
    """
    Verify that "element" is an event-range-with-margin
    """
    assert_tag(element, 'table')
    assert_class(element, 'event-range-with-margin')
    tr = element.find('xhtml:tr', ns)
    assert tr is not None
    td = tr.find('xhtml:td', ns)
    assert_class(td, 'event-range')

    events_hdr = td.find('xhtml:div', ns)
    assert_class(events_hdr, 'events-hdr')

    #...etc

def get_diag_by_index(html_tree, index):
    root = html_tree.getroot ()
    assert root.tag == make_tag('html')

    body = root.find('xhtml:body', ns)
    assert body is not None

    diag_list = body.find('xhtml:div', ns)
    assert diag_list is not None
    assert_class(diag_list, 'gcc-diagnostic-list')

    diags = diag_list.findall('xhtml:div', ns)
    diag = diags[index]
    assert_class(diag, 'gcc-diagnostic')
    return diag

def get_message_within_diag(diag_element):
    msg = diag_element.find('xhtml:span', ns)
    assert_class(msg, 'gcc-message')
    return msg

def get_locus_within_diag(diag_element):
    src = diag_element.find('xhtml:table', ns)
    assert_class(src, 'locus')
    return src
