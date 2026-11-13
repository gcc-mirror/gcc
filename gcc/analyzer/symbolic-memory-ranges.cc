/* A (base_region, symbolic bit range) pair.
   Copyright (C) 2026 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "analyzer/common.h"

#include "selftest.h"

#include "analyzer/symbolic-memory-ranges.h"
#include "analyzer/region-model-manager.h"

#if ENABLE_ANALYZER

namespace ana {
namespace symbolic {

// class memory_range

memory_range::memory_range (pointer sym_ptr,
			    const symbolic_byte_size &num_bytes)
: m_base_reg (sym_ptr.get_base_region ()),
  m_bit_range (bits_from_bytes (symbolic_byte_range (sym_ptr.get_byte_offset (),
						     num_bytes)))
{
}

DEBUG_FUNCTION void
memory_range::dump () const
{
  symbolic::dump_node_tree_to_stderr<memory_range> (*this);
}

std::unique_ptr<text_art::tree_widget>
memory_range::make_dump_widget (symbolic::node_dump_state &nds) const
{
  using text_art::tree_widget;
  std::unique_ptr<tree_widget> widget
    (tree_widget::from_fmt (nds, nullptr, "Memory Range"));
  widget->add_child (m_base_reg.make_dump_widget (nds, "base region"));
  widget->add_child (m_bit_range.make_dump_widget (nds, "bit range"));
  return widget;
}

std::unique_ptr<json::object>
memory_range::to_json () const
{
  auto obj = std::make_unique<json::object> ();
  obj->set ("base_reg", m_base_reg.to_json ());
  obj->set ("bit_range", m_bit_range.to_json ());
  return obj;
}

} // namespace ana::symbolic

#if CHECKING_P

namespace selftest {

/* seen in test_29c at
   q = &p[7];
   __analyzer_eval (q->x == 107024);

(gdb) call memory_range.dump()
Memory Range
├─ base region: (635): ‘struct coord’: symbolic_region: %<*%>
│  ├─ m_sval_ptr: (630): ‘struct coord *’: pointer_svalue:
│  │  ╰─ Pointer
│  │     ├─ base region: (583): alloca_region
│  │     │  ╰─ parent: (520): frame_region(‘test_29c’, index: 0, depth: 1)
│  │     │     ╰─ parent: (1): stack region
│  │     │        ╰─ parent: (0): root region
│  │     ╰─ byte offset: (629): scalar<bytes>: 112
│  ╰─ parent: (0): root region
╰─ bit range: symbolic range
   ├─ start offset: (527): scalar<bits>: 0
   ╰─ size: (528): scalar<bits>: 64

Presumably we should fold this to:

Memory Range
├─ base region: (583): alloca_region
│  ╰─ parent: (520): frame_region(‘test_29c’, index: 0, depth: 1)
│     ╰─ parent: (1): stack region
│        ╰─ parent: (0): root region
╰─ bit range: symbolic range
   ├─ start offset: (527): scalar<bits>: 112 * 8
   ╰─ size: (528): scalar<bits>: 64

i.e.
  memory_range (SYMBOLIC(POINTER(BASE_REG, OFFSET), (START, SIZE)))
should fold to:
  memory_range (BASE_REG, (START + OFFSET, SIZE))
*/

static void
test_offset_from_pointer ()
{
  symbolic::node_manager mgr;
  auto &bv_mgr = mgr.get_bit_vector_manager ();
  auto &rmm = mgr.get_region_model_manager ();


  // coord_test ct;
  // FIXME
}
  
/* Run all of the selftests within this file.  */

void
analyzer_symbolic_memory_ranges_cc_tests ()
{
  test_offset_from_pointer ();
}

} // namespace selftest

#endif /* CHECKING_P */

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
