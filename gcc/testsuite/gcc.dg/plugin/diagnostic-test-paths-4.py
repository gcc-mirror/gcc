# Verify that interprocedural execution paths work in HTML output.

from htmltest import *

import pytest

@pytest.fixture(scope='function', autouse=True)
def html_tree():
    return html_tree_from_env()

def test_paths(html_tree):
    diag = get_diag_by_index(html_tree, 0)
    src = get_locus_within_diag (diag)

    tbody = src.find('xhtml:tbody', ns)
    assert_class(tbody, 'line-span')

    rows = tbody.findall('xhtml:tr', ns)

    quoted_src_tr = rows[0]
    assert_quoted_line(quoted_src_tr,
                       '   13', '  fprintf(stderr, "LOG: %s", msg); /* { dg-warning "call to \'fprintf\' from within signal handler" } */')
    
    annotation_tr = rows[1]
    assert_annotation_line(annotation_tr,
                           '  ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')

    event_ranges = diag.find('xhtml:div', ns)
    assert_class(event_ranges, 'event-ranges')

    test_frame_margin = event_ranges.find('xhtml:table', ns)
    assert_class(test_frame_margin, 'stack-frame-with-margin')

    tr = test_frame_margin.find('xhtml:tr', ns)
    assert tr is not None
    tds = tr.findall('xhtml:td', ns)
    assert len(tds) == 2

    assert_class(tds[0], 'interprocmargin')

    test_frame = tds[1]
    assert_frame(test_frame, 'test')
    assert_event_range_with_margin(test_frame[1])

# For reference, generated HTML looks like this:
"""
<table class="stack-frame-with-margin"><tr>
  <td class="interprocmargin" style="padding-left: 100px"/>
  <td class="stack-frame">
<div class="frame-funcname"><span>test</span></div><table class="event-range-with-margin"><tr>
  <td class="event-range">
    <div class="events-hdr"><span class="funcname">test</span>: <span class="event-ids">events 1-2</span></div>
<table class="locus">
<tbody class="line-span">
<tr><td class="linenum">   27</td> <td class="source">{</td></tr>
<tr><td class="linenum"/><td class="annotation">^</td></tr>
<tr><td class="linenum"/><td class="annotation">|</td></tr>
<tr><td class="linenum"/><td class="annotation">(1) entering 'test'</td></tr>
<tr><td class="linenum">   28</td> <td class="source">  register_handler ();</td></tr>
<tr><td class="linenum"/><td class="annotation">  ~~~~~~~~~~~~~~~~~~~</td></tr>
<tr><td class="linenum"/><td class="annotation">  |</td></tr>
<tr><td class="linenum"/><td class="annotation">  (2) calling 'register_handler'</td></tr>
</tbody>
</table>
</td></tr></table>
<div class="between-ranges-call">
  <svg height="30" width="150">
    <defs>
      <marker id="arrowhead" markerWidth="10" markerHeight="7"
              refX="0" refY="3.5" orient="auto" stroke="#0088ce" fill="#0088ce">
      <polygon points="0 0, 10 3.5, 0 7"/>
      </marker>
    </defs>
    <polyline points="20,0 20,10 120,10 120,20"
              style="fill:none;stroke: #0088ce"
              marker-end="url(#arrowhead)"/>
  </svg>
</div>

<table class="stack-frame-with-margin"><tr>
  <td class="interprocmargin" style="padding-left: 100px"/>
  <td class="stack-frame">
<div class="frame-funcname"><span>register_handler</span></div><table class="event-range-with-margin"><tr>
  <td class="event-range">
    <div class="events-hdr"><span class="funcname">register_handler</span>: <span class="event-ids">events 3-4</span></div>
<table class="locus">
<tbody class="line-span">
<tr><td class="linenum">   22</td> <td class="source">{</td></tr>
<tr><td class="linenum"/><td class="annotation">^</td></tr>
<tr><td class="linenum"/><td class="annotation">|</td></tr>
<tr><td class="linenum"/><td class="annotation">(3) entering 'register_handler'</td></tr>
<tr><td class="linenum">   23</td> <td class="source">  signal(SIGINT, int_handler);</td></tr>
<tr><td class="linenum"/><td class="annotation">  ~~~~~~~~~~~~~~~~~~~~~~~~~~~</td></tr>
<tr><td class="linenum"/><td class="annotation">  |</td></tr>
<tr><td class="linenum"/><td class="annotation">  (4) registering 'int_handler' as signal handler</td></tr>
</tbody>
</table>
</td></tr></table>
</td></tr></table>
</td></tr></table>
<div class="between-ranges-return">
  <svg height="30" width="250">
    <defs>
      <marker id="arrowhead" markerWidth="10" markerHeight="7"
              refX="0" refY="3.5" orient="auto" stroke="#0088ce" fill="#0088ce">
      <polygon points="0 0, 10 3.5, 0 7"/>
      </marker>
    </defs>
    <polyline points="220,0 220,10 20,10 20,20"
              style="fill:none;stroke: #0088ce"
              marker-end="url(#arrowhead)"/>
  </svg>
</div>

<table class="event-range-with-margin"><tr>
  <td class="event-range">
    <div class="events-hdr"><span class="event-ids">event 5</span></div>
 (5): later on, when the signal is delivered to the process
</td></tr></table>
<div class="between-ranges-call">
  <svg height="30" width="150">
    <defs>
      <marker id="arrowhead" markerWidth="10" markerHeight="7"
              refX="0" refY="3.5" orient="auto" stroke="#0088ce" fill="#0088ce">
      <polygon points="0 0, 10 3.5, 0 7"/>
      </marker>
    </defs>
    <polyline points="20,0 20,10 120,10 120,20"
              style="fill:none;stroke: #0088ce"
              marker-end="url(#arrowhead)"/>
  </svg>
</div>

<table class="stack-frame-with-margin"><tr>
  <td class="interprocmargin" style="padding-left: 100px"/>
  <td class="stack-frame">
<div class="frame-funcname"><span>int_handler</span></div><table class="event-range-with-margin"><tr>
  <td class="event-range">
    <div class="events-hdr"><span class="funcname">int_handler</span>: <span class="event-ids">events 6-7</span></div>
<table class="locus">
<tbody class="line-span">
<tr><td class="linenum">   17</td> <td class="source">{</td></tr>
<tr><td class="linenum"/><td class="annotation">^</td></tr>
<tr><td class="linenum"/><td class="annotation">|</td></tr>
<tr><td class="linenum"/><td class="annotation">(6) entering 'int_handler'</td></tr>
<tr><td class="linenum">   18</td> <td class="source">  custom_logger("got signal");</td></tr>
<tr><td class="linenum"/><td class="annotation">  ~~~~~~~~~~~~~~~~~~~~~~~~~~~</td></tr>
<tr><td class="linenum"/><td class="annotation">  |</td></tr>
<tr><td class="linenum"/><td class="annotation">  (7) calling 'custom_logger'</td></tr>
</tbody>
</table>
</td></tr></table>
<div class="between-ranges-call">
  <svg height="30" width="150">
    <defs>
      <marker id="arrowhead" markerWidth="10" markerHeight="7"
              refX="0" refY="3.5" orient="auto" stroke="#0088ce" fill="#0088ce">
      <polygon points="0 0, 10 3.5, 0 7"/>
      </marker>
    </defs>
    <polyline points="20,0 20,10 120,10 120,20"
              style="fill:none;stroke: #0088ce"
              marker-end="url(#arrowhead)"/>
  </svg>
</div>

<table class="stack-frame-with-margin"><tr>
  <td class="interprocmargin" style="padding-left: 100px"/>
  <td class="stack-frame">
<div class="frame-funcname"><span>custom_logger</span></div><table class="event-range-with-margin"><tr>
  <td class="event-range">
    <div class="events-hdr"><span class="funcname">custom_logger</span>: <span class="event-ids">events 8-9</span></div>
<table class="locus">
<tbody class="line-span">
<tr><td class="linenum">   12</td> <td class="source">{</td></tr>
<tr><td class="linenum"/><td class="annotation">^</td></tr>
<tr><td class="linenum"/><td class="annotation">|</td></tr>
<tr><td class="linenum"/><td class="annotation">(8) entering 'custom_logger'</td></tr>
<tr><td class="linenum">   13</td> <td class="source">  fprintf(stderr, "LOG: %s", msg); /* { dg-warning "call to 'fprintf' from within signal handler" } */</td></tr>
<tr><td class="linenum"/><td class="annotation">  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~</td></tr>
<tr><td class="linenum"/><td class="annotation">  |</td></tr>
<tr><td class="linenum"/><td class="annotation">  (9) calling 'fprintf'</td></tr>
</tbody>
</table>
</td></tr></table>
</td></tr></table>
</td></tr></table>

</div>
    """
