/* Text art visualizations within -fanalyzer.
   Copyright (C) 2023-2025 Free Software Foundation, Inc.

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

#include "config.h"
#define INCLUDE_ALGORITHM
#define INCLUDE_MAP
#define INCLUDE_SET
#define INCLUDE_VECTOR
#include "system.h"
#include "coretypes.h"
#include "coretypes.h"
#include "tree.h"
#include "function.h"
#include "basic-block.h"
#include "gimple.h"
#include "diagnostic-core.h"
#include "diagnostic.h"
#include "intl.h"
#include "make-unique.h"
#include "tree-diagnostic.h" /* for default_tree_printer.  */
#include "analyzer/analyzer.h"
#include "analyzer/region-model.h"
#include "analyzer/access-diagram.h"
#include "text-art/ruler.h"
#include "fold-const.h"
#include "analyzer/analyzer-selftests.h"

#if ENABLE_ANALYZER

/* Consider this code:
     int32_t arr[10];
     arr[10] = x;
   where we've emitted a buffer overflow diagnostic like this:
     out-of-bounds write from byte 40 till byte 43 but 'arr' ends at byte 40

   We want to emit a diagram that visualizes:
   - the spatial relationship between the valid region to access, versus
   the region that was actually accessed: does it overlap, was it touching,
   close, or far away?  Was it before or after in memory?  What are the
   relative sizes involved?
   - the direction of the access (read vs write)

   The following code supports emitting diagrams similar to the following:

   #                                        +--------------------------------+
   #                                        |write from ‘x’ (type: ‘int32_t’)|
   #                                        +--------------------------------+
   #                                                        |
   #                                                        |
   #                                                        v
   #  +---------+-----------+-----------+   +--------------------------------+
   #  |   [0]   |    ...    |    [9]    |   |       after valid range        |
   #  +---------+-----------+-----------+   |                                |
   #  |   ‘arr’ (type: ‘int32_t[10]’)   |   |                                |
   #  +---------------------------------+   +--------------------------------+
   #  |~~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~~|   |~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~~|
   #                   |                                    |
   #         +---------+--------+                 +---------+---------+
   #         |capacity: 40 bytes|                 |overflow of 4 bytes|
   #         +------------------+                 +-------------------+

  where the diagram is laid out via table columns where each table column
  represents either a range of bits/bytes, or is a spacing column (to highlight
  the boundary between valid vs invalid accesses).  The table columns can be
  seen via -fanalyzer-debug-text-art.  For example, here there are 5 table
  columns ("tc0" through "tc4"):

   #  +---------+-----------+-----------+---+--------------------------------+
   #  |   tc0   |    tc1    |    tc2    |tc3|              tc4               |
   #  +---------+-----------+-----------+---+--------------------------------+
   #  |bytes 0-3|bytes 4-35 |bytes 36-39|   |          bytes 40-43           |
   #  +---------+-----------+-----------+   +--------------------------------+
   #
   #                                        +--------------------------------+
   #                                        |write from ‘x’ (type: ‘int32_t’)|
   #                                        +--------------------------------+
   #                                                        |
   #                                                        |
   #                                                        v
   #  +---------+-----------+-----------+   +--------------------------------+
   #  |   [0]   |    ...    |    [9]    |   |       after valid range        |
   #  +---------+-----------+-----------+   |                                |
   #  |   ‘arr’ (type: ‘int32_t[10]’)   |   |                                |
   #  +---------------------------------+   +--------------------------------+
   #  |~~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~~|   |~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~~|
   #                   |                                    |
   #         +---------+--------+                 +---------+---------+
   #         |capacity: 40 bytes|                 |overflow of 4 bytes|
   #         +------------------+                 +-------------------+

  The diagram is built up from the following:

   #                                        +--------------------------------+
   #                                        | ITEM FOR SVALUE/ACCESSED REGION|
   #                                        +--------------------------------+
   #                                                        |
   #                                                        | DIRECTION WIDGET
   #                                                        v
   #  +---------------------------------+   +--------------------------------+
   #  |   VALID REGION                  |   | INVALID ACCESS                 |
   #  +---------------------------------+   +--------------------------------+
   #
   #  |                       VALID-VS-INVALID RULER                         |

  i.e. a vbox_widget containing 4 child widgets laid out vertically:
  - ALIGNED CHILD WIDGET: ITEM FOR SVALUE/ACCESSED REGION
  - DIRECTION WIDGET
  - ALIGNED CHILD WIDGET: VALID AND INVALID ACCESSES
  - VALID-VS-INVALID RULER.

  A more complicated example, given this overflow:
     char buf[100];
     strcpy (buf, LOREM_IPSUM);

   01| +---+---+---+---+---+---+----------+-----+-----+-----+-----+-----+-----+
   02| |[0]|[1]|[2]|[3]|[4]|[5]|   ...    |[440]|[441]|[442]|[443]|[444]|[445]|
   03| +---+---+---+---+---+---+          +-----+-----+-----+-----+-----+-----+
   04| |'L'|'o'|'r'|'e'|'m'|' '|          | 'o' | 'r' | 'u' | 'm' | '.' | NUL |
   05| +---+---+---+---+---+---+----------+-----+-----+-----+-----+-----+-----+
   06| |                  string literal (type: 'char[446]')                  |
   07| +----------------------------------------------------------------------+
   08|   |   |   |   |   |   |  |  |    |    |     |     |     |     |     |
   09|   |   |   |   |   |   |  |  |    |    |     |     |     |     |     |
   10|   v   v   v   v   v   v  v  v    v    v     v     v     v     v     v
   11| +---+---------------------+----++--------------------------------------+
   12| |[0]|         ...         |[99]||          after valid range           |
   13| +---+---------------------+----+|                                      |
   14| |  'buf' (type: 'char[100]')   ||                                      |
   15| +------------------------------++--------------------------------------+
   16| |~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~||~~~~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~~~~~|
   17|                |                                   |
   18|      +---------+---------+              +----------+----------+
   19|      |capacity: 100 bytes|              |overflow of 346 bytes|
   20|      +-------------------+              +---------------------+

 which is:

   01| ALIGNED CHILD WIDGET (lines 01-07): (string_region_spatial_item)-+-----+
   02| |[0]|[1]|[2]|[3]|[4]|[5]|   ...    |[440]|[441]|[442]|[443]|[444]|[445]|
   03| +---+---+---+---+---+---+          +-----+-----+-----+-----+-----+-----+
   04| |'L'|'o'|'r'|'e'|'m'|' '|          | 'o' | 'r' | 'u' | 'm' | '.' | NUL |
   05| +---+---+---+---+---+---+----------+-----+-----+-----+-----+-----+-----+
   06| |                  string literal (type: 'char[446]')                  |
   07| +----------------------------------------------------------------------+
   08| DIRECTION WIDGET (lines 08-10)   |    |     |     |     |     |     |
   09|   |   |   |   |   |   |  |  |    |    |     |     |     |     |     |
   10|   v   v   v   v   v   v  v  v    v    v     v     v     v     v     v
   11| ALIGNED CHILD WIDGET (lines 11-15)-------------------------------------+
   12| VALID REGION  ...         |[99]|| INVALID ACCESS                       |
   13| +---+---------------------+----+|                                      |
   14| |  'buf' (type: 'char[100]')   ||                                      |
   15| +------------------------------++--------------------------------------+
   16| VALID-VS-INVALID RULER (lines 16-20): ~~~~~~~~~~~~~+~~~~~~~~~~~~~~~~~~~|
   17|                |                                   |
   18|      +---------+---------+              +----------+----------+
   19|      |capacity: 100 bytes|              |overflow of 346 bytes|
   20|      +-------------------+              +---------------------+

   We build the diagram in several phases:
   - (1) we construct an access_diagram_impl widget.  Within the ctor, we have
   these subphases:
   -   (1.1) find all of the boundaries of interest
   -   (1.2) use the boundaries to build a bit_table_map, associating bit ranges
   with table columns (e.g. "byte 0 is column 0, bytes 1-98 are column 2" etc)
   -   (1.3) create child widgets that share this table-based geometry
   - (2) ask the widget for its size request
   -   (2.1) column widths and row heights for the table are computed by
   access_diagram_impl::calc_req_size
   -   (2.2) child widgets request sizes based on these widths/heights
   - (3) create a canvas of the appropriate size
   - (4) paint the widget hierarchy to the canvas.  */


using namespace text_art;

namespace ana {

static styled_string
fmt_styled_string (style_manager &sm,
		   const char *fmt, ...)
    ATTRIBUTE_GCC_DIAG(2, 3);

static styled_string
fmt_styled_string (style_manager &sm,
		   const char *fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);
  styled_string result
    = styled_string::from_fmt_va (sm, default_tree_printer, fmt, &ap);
  va_end (ap);
  return result;
}

class access_diagram_impl;
class bit_to_table_map;

static void
pp_bit_size_t (pretty_printer *pp, bit_size_t num_bits)
{
  if (num_bits % BITS_PER_UNIT == 0)
    {
      byte_size_t num_bytes = num_bits / BITS_PER_UNIT;
      if (num_bytes == 1)
	pp_printf (pp, _("%wi byte"), num_bytes.to_uhwi ());
      else
	pp_printf (pp, _("%wi bytes"), num_bytes.to_uhwi ());
    }
  else
    {
      if (num_bits == 1)
	pp_printf (pp, _("%wi bit"), num_bits.to_uhwi ());
      else
	pp_printf (pp, _("%wi bits"), num_bits.to_uhwi ());
    }
}

static styled_string
get_access_size_str (style_manager &sm,
		     const access_operation &op,
		     access_range accessed_range,
		     tree type)
{
  bit_size_expr num_bits (accessed_range.get_size (op.m_model.get_manager ()));
  if (type)
    {
      styled_string s;
      pretty_printer pp;
      pp_format_decoder (&pp) = default_tree_printer;
      if (num_bits.maybe_print_for_user (&pp, op.m_model))
	{
	  if (op.m_dir == DIR_READ)
	    return fmt_styled_string (sm,
				      _("read of %qT (%s)"),
				      type,
				      pp_formatted_text (&pp));
	  else
	    return fmt_styled_string (sm,
				      _("write of %qT (%s)"),
				      type,
				      pp_formatted_text (&pp));
	}
    }
  if (op.m_dir == DIR_READ)
    {
      if (auto p
	  = num_bits.maybe_get_formatted_str (sm, op.m_model,
					      _("read of %wi bit"),
					      _("read of %wi bits"),
					      _("read of %wi byte"),
					      _("read of %wi bytes"),
					      _("read of %qs bits"),
					      _("read of %qs bytes")))
	return std::move (*p.get ());
    }
  else
    {
      if (auto p
	  = num_bits.maybe_get_formatted_str (sm, op.m_model,
					      _("write of %wi bit"),
					      _("write of %wi bits"),
					      _("write of %wi byte"),
					      _("write of %wi bytes"),
					      _("write of %qs bits"),
					      _("write of %qs bytes")))
	return std::move (*p.get ());
    }

  if (type)
    {
      if (op.m_dir == DIR_READ)
	return fmt_styled_string (sm, _("read of %qT"), type);
      else
	return fmt_styled_string (sm, _("write of %qT"), type);
    }

  if (op.m_dir == DIR_READ)
    return styled_string (sm, _("read"));
  else
    return styled_string (sm, _("write"));
}

/* Subroutine of clean_up_for_diagram.  */

static tree
strip_any_cast (tree expr)
{
  if (TREE_CODE (expr) == NOP_EXPR
      || TREE_CODE (expr) == NON_LVALUE_EXPR)
    expr = TREE_OPERAND (expr, 0);
  return expr;
}

/* Duplicate EXPR, replacing any SSA names with the underlying variable.  */

tree
remove_ssa_names (tree expr)
{
  if (TREE_CODE (expr) == SSA_NAME
      && SSA_NAME_VAR (expr))
    return SSA_NAME_VAR (expr);
  tree t = copy_node (expr);
  for (int i = 0; i < TREE_OPERAND_LENGTH (expr); i++)
    if (TREE_OPERAND (expr, i))
      TREE_OPERAND (t, i) = remove_ssa_names (TREE_OPERAND (expr, i));
  return t;
}

/* We want to be able to print tree expressions from the analyzer,
   which is in the middle end.

   We could use the front-end pretty_printer's formatting routine,
   but:
   (a) some have additional state in a pretty_printer subclass, so we'd
   need to clone global_dc->printer
   (b) the "aka" type information added by the C and C++ frontends are
   too verbose when building a diagram, and there isn't a good way to ask
   for a less verbose version of them.

   Hence we use default_tree_printer.
   However, we want to avoid printing SSA names, and instead print the
   underlying var name.
   Ideally there would be a better tree printer for use by middle end
   warnings, but as workaround, this function clones a tree, replacing
   SSA names with the var names.  */

tree
clean_up_for_diagram (tree expr)
{
  tree without_ssa_names = remove_ssa_names (expr);
  return strip_any_cast (without_ssa_names);
}

/* struct bit_size_expr.  */

/* Attempt to generate a user-facing styled string that mentions this
   bit_size_expr.
   Use MODEL for extracting representative tree values where necessary.
   The CONCRETE_* format strings should contain a single %wi.
   The SYMBOLIC_* format strings should contain a single %qs.
   Return nullptr if unable to represent the expression.  */

std::unique_ptr<text_art::styled_string>
bit_size_expr::maybe_get_formatted_str (text_art::style_manager &sm,
					const region_model &model,
					const char *concrete_single_bit_fmt,
					const char *concrete_plural_bits_fmt,
					const char *concrete_single_byte_fmt,
					const char *concrete_plural_bytes_fmt,
					const char *symbolic_bits_fmt,
					const char *symbolic_bytes_fmt) const
{
  region_model_manager &mgr = *model.get_manager ();
  if (const svalue *num_bytes = maybe_get_as_bytes (mgr))
    {
      if (tree cst = num_bytes->maybe_get_constant ())
	{
	  byte_size_t concrete_num_bytes = wi::to_offset (cst);
	  if (!wi::fits_uhwi_p (concrete_num_bytes))
	    return nullptr;
	  if (concrete_num_bytes == 1)
	    return ::make_unique <text_art::styled_string>
	      (fmt_styled_string (sm, concrete_single_byte_fmt,
				  concrete_num_bytes.to_uhwi ()));
	  else
	    return ::make_unique <text_art::styled_string>
	      (fmt_styled_string (sm, concrete_plural_bytes_fmt,
				  concrete_num_bytes.to_uhwi ()));
	}
      else
	{
	  pretty_printer pp;
	  pp_format_decoder (&pp) = default_tree_printer;
	  if (!num_bytes->maybe_print_for_user (&pp, model))
	    return nullptr;
	  return ::make_unique <text_art::styled_string>
	    (fmt_styled_string (sm, symbolic_bytes_fmt,
				pp_formatted_text (&pp)));
	}
    }
  else if (tree cst = m_num_bits.maybe_get_constant ())
    {
      bit_size_t concrete_num_bits = wi::to_offset (cst);
      if (!wi::fits_uhwi_p (concrete_num_bits))
	return nullptr;
      if (concrete_num_bits == 1)
	return ::make_unique <text_art::styled_string>
	  (fmt_styled_string (sm, concrete_single_bit_fmt,
			      concrete_num_bits.to_uhwi ()));
      else
	return ::make_unique <text_art::styled_string>
	  (fmt_styled_string (sm, concrete_plural_bits_fmt,
			      concrete_num_bits.to_uhwi ()));
    }
  else
    {
      pretty_printer pp;
      pp_format_decoder (&pp) = default_tree_printer;
      if (!m_num_bits.maybe_print_for_user (&pp, model))
	return nullptr;
      return ::make_unique <text_art::styled_string>
	(fmt_styled_string (sm, symbolic_bits_fmt,
			    pp_formatted_text (&pp)));
    }
}

bool
bit_size_expr::maybe_print_for_user (pretty_printer *pp,
				     const region_model &model) const
{
  if (tree cst = m_num_bits.maybe_get_constant ())
    {
      bit_size_t concrete_num_bits = wi::to_offset (cst);
      pp_bit_size_t (pp, concrete_num_bits);
      return true;
    }
  else
    {
      if (const svalue *num_bytes = maybe_get_as_bytes (*model.get_manager ()))
	{
	  pretty_printer tmp_pp;
	  pp_format_decoder (&tmp_pp) = default_tree_printer;
	  if (!num_bytes->maybe_print_for_user (&tmp_pp, model))
	    return false;
	  pp_printf (pp, _("%qs bytes"), pp_formatted_text (&tmp_pp));
	  return true;
	}
      else
	{
	  pretty_printer tmp_pp;
	  pp_format_decoder (&tmp_pp) = default_tree_printer;
	  if (!m_num_bits.maybe_print_for_user (&tmp_pp, model))
	    return false;
	  pp_printf (pp, _("%qs bits"), pp_formatted_text (&tmp_pp));
	  return true;
	}
    }
}

/* Attempt to get a symbolic value for this symbolic bit size,
   expressed in bytes.
   Return null if it's not known to divide exactly.  */

const svalue *
bit_size_expr::maybe_get_as_bytes (region_model_manager &mgr) const
{
  if (tree cst = m_num_bits.maybe_get_constant ())
    {
      bit_offset_t concrete_bits = wi::to_offset (cst);
      if (concrete_bits % BITS_PER_UNIT != 0)
	/* Not an exact multiple, so fail.  */
	return nullptr;
    }
  const svalue *bits_per_byte
    = mgr.get_or_create_int_cst (NULL_TREE, BITS_PER_UNIT);
  return mgr.maybe_fold_binop (NULL_TREE, EXACT_DIV_EXPR,
			       &m_num_bits, bits_per_byte);
}

/* struct access_range.  */

access_range::access_range (const region *base_region, const bit_range &bits)
: m_start (region_offset::make_concrete (base_region,
					 bits.get_start_bit_offset ())),
  m_next (region_offset::make_concrete (base_region,
					bits.get_next_bit_offset ()))
{
}

access_range::access_range (const region *base_region, const byte_range &bytes)
: m_start (region_offset::make_concrete (base_region,
					 bytes.get_start_bit_offset ())),
  m_next (region_offset::make_concrete (base_region,
					bytes.get_next_bit_offset ()))
{
}

access_range::access_range (const region &reg, region_model_manager *mgr)
: m_start (strip_types (reg.get_offset (mgr), *mgr)),
  m_next (strip_types (reg.get_next_offset (mgr), *mgr))
{
}

bit_size_expr
access_range::get_size (region_model_manager *mgr) const
{
  const svalue &start_bit_offset = m_start.calc_symbolic_bit_offset (mgr);
  const svalue &next_bit_offset = m_next.calc_symbolic_bit_offset (mgr);
  return bit_size_expr
    (*mgr->get_or_create_binop (NULL_TREE, MINUS_EXPR,
				&next_bit_offset, &start_bit_offset));
}

bool
access_range::contains_p (const access_range &other) const
{
  return (m_start <= other.m_start
	  && other.m_next <= m_next);
}

bool
access_range::empty_p () const
{
  bit_range concrete_bits (0, 0);
  if (!as_concrete_bit_range (&concrete_bits))
    return false;
  return concrete_bits.empty_p ();
}

void
access_range::dump_to_pp (pretty_printer *pp, bool simple) const
{
  if (m_start.concrete_p () && m_next.concrete_p ())
    {
      bit_range bits (m_start.get_bit_offset (),
		      m_next.get_bit_offset () - m_start.get_bit_offset ());
      bits.dump_to_pp (pp);
      return;
    }
  pp_character (pp, '[');
  m_start.dump_to_pp (pp, simple);
  pp_string (pp, " to ");
  m_next.dump_to_pp (pp, simple);
  pp_character (pp, ')');
}

DEBUG_FUNCTION void
access_range::dump (bool simple) const
{
  tree_dump_pretty_printer pp (stderr);
  dump_to_pp (&pp, simple);
  pp_newline (&pp);
}

void
access_range::log (const char *title, logger &logger) const
{
  logger.start_log_line ();
  logger.log_partial ("%s: ", title);
  dump_to_pp (logger.get_printer (), true);
  logger.end_log_line ();
}

/* struct access_operation.  */

access_range
access_operation::get_valid_bits () const
{
  const svalue *capacity_in_bytes_sval = m_model.get_capacity (m_base_region);
  return access_range
    (region_offset::make_concrete (m_base_region, 0),
     region_offset::make_byte_offset (m_base_region, capacity_in_bytes_sval),
     *get_manager ());
}

access_range
access_operation::get_actual_bits () const
{
  return access_range (m_reg, get_manager ());
}

/* If there are any bits accessed invalidly before the valid range,
   return true and write their range to *OUT.
   Return false if there aren't, or if there's a problem
   (e.g. symbolic ranges.  */

bool
access_operation::maybe_get_invalid_before_bits (access_range *out) const
{
  access_range valid_bits (get_valid_bits ());
  access_range actual_bits (get_actual_bits ());

  if (actual_bits.m_start >= valid_bits.m_start)
    {
      /* No part of accessed range is before the valid range.  */
      return false;
    }
  else if (actual_bits.m_next > valid_bits.m_start)
    {
      /* Get part of accessed range that's before the valid range.  */
      *out = access_range (actual_bits.m_start, valid_bits.m_start,
			   *get_manager ());
      return true;
    }
  else
    {
      /* Accessed range is fully before valid range.  */
      *out = actual_bits;
      return true;
    }
}

/* If there are any bits accessed invalidly after the valid range,
   return true and write their range to *OUT.
   Return false if there aren't, or if there's a problem.  */

bool
access_operation::maybe_get_invalid_after_bits (access_range *out) const
{
  access_range valid_bits (get_valid_bits ());
  access_range actual_bits (get_actual_bits ());

  if (actual_bits.m_next <= valid_bits.m_next)
    {
      /* No part of accessed range is after the valid range.  */
      return false;
    }
  else if (actual_bits.m_start < valid_bits.m_next)
    {
      /* Get part of accessed range that's after the valid range.  */
      *out = access_range (valid_bits.m_next, actual_bits.m_next,
			   *get_manager ());
      return true;
    }
  else
    {
      /* Accessed range is fully after valid range.  */
      *out = actual_bits;
      return true;
    }
}

/* A class for capturing all of the region offsets of interest (both concrete
   and symbolic), to help align everything in the diagram.
   Boundaries can be soft or hard; hard boundaries are emphasized visually
   (e.g. the boundary between valid vs invalid accesses).

   Offsets in the boundaries are all expressed relative to the base
   region of the access_operation.  */

class boundaries
{
public:
  enum class kind { HARD, SOFT};

  boundaries (const region &base_reg, logger *logger)
  : m_base_reg (base_reg), m_logger (logger)
  {
  }

  void add (region_offset offset, enum kind k)
  {
    m_all_offsets.insert (offset);
    if (k == kind::HARD)
      m_hard_offsets.insert (offset);
  }

  void add (const access_range &range, enum kind kind)
  {
    add (range.m_start, kind);
    add (range.m_next, kind);
    if (m_logger)
      {
	m_logger->start_log_line ();
	m_logger->log_partial ("added access_range: ");
	range.dump_to_pp (m_logger->get_printer (), true);
	m_logger->log_partial (" (%s)",
			       (kind == boundaries::kind::HARD)
			       ? "HARD" : "soft");
	m_logger->end_log_line ();
      }
  }

  void add (const region &reg, region_model_manager *mgr, enum kind kind)
  {
    add (access_range (reg.get_offset (mgr),
		       reg.get_next_offset (mgr),
		       *mgr),
	 kind);
  }

  void add (const byte_range bytes, enum kind kind)
  {
    add (access_range (&m_base_reg, bytes), kind);
  }

  void add_all_bytes_in_range (const byte_range &bytes)
  {
    for (byte_offset_t byte_idx = bytes.get_start_byte_offset ();
	 byte_idx <= bytes.get_next_byte_offset ();
	 byte_idx = byte_idx + 1)
      add (region_offset::make_concrete (&m_base_reg, byte_idx * 8),
	   kind::SOFT);
  }

  void add_all_bytes_in_range (const access_range &range)
  {
    byte_range bytes (0, 0);
    bool valid = range.as_concrete_byte_range (&bytes);
    gcc_assert (valid);
    add_all_bytes_in_range (bytes);
  }

  void log (logger &logger) const
  {
    logger.log ("boundaries:");
    logger.inc_indent ();
    for (auto offset : m_all_offsets)
      {
	enum kind k = get_kind (offset);
	logger.start_log_line ();
	logger.log_partial ("%s: ", (k == kind::HARD) ? "HARD" : "soft");
	offset.dump_to_pp (logger.get_printer (), true);
	logger.end_log_line ();
      }
    logger.dec_indent ();
  }

  enum kind get_kind (region_offset offset) const
  {
    gcc_assert (m_all_offsets.find (offset) != m_all_offsets.end ());
    if (m_hard_offsets.find (offset) != m_hard_offsets.end ())
      return kind::HARD;
    else
      return kind::SOFT;
  }

  std::set<region_offset>::const_iterator begin () const
  {
    return m_all_offsets.begin ();
  }
  std::set<region_offset>::const_iterator end () const
  {
    return m_all_offsets.end ();
  }
  std::set<region_offset>::size_type size () const
  {
    return m_all_offsets.size ();
  }

  std::vector<region_offset>
  get_hard_boundaries_in_range (byte_offset_t min_offset,
				byte_offset_t max_offset) const
  {
    std::vector<region_offset> result;
    for (auto &offset : m_hard_offsets)
      {
	if (!offset.concrete_p ())
	  continue;
	byte_offset_t byte;
	if (!offset.get_concrete_byte_offset (&byte))
	  continue;
	if (byte < min_offset)
	  continue;
	if (byte > max_offset)
	  continue;
	result.push_back (offset);
      }
    return result;
  }

private:
  const region &m_base_reg;
  logger *m_logger;
  std::set<region_offset> m_all_offsets;
  std::set<region_offset> m_hard_offsets;
};

/* A widget that wraps a table but offloads column-width calculation
   to a shared object, so that we can vertically line up multiple tables
   and have them all align their columns.

   For example, in:

   01| +---+---+---+---+---+---+----------+-----+-----+-----+-----+-----+-----+
   02| |[0]|[1]|[2]|[3]|[4]|[5]|   ...    |[440]|[441]|[442]|[443]|[444]|[445]|
   03| +---+---+---+---+---+---+          +-----+-----+-----+-----+-----+-----+
   04| |'L'|'o'|'r'|'e'|'m'|' '|          | 'o' | 'r' | 'u' | 'm' | '.' | NUL |
   05| +---+---+---+---+---+---+----------+-----+-----+-----+-----+-----+-----+
   06| |                  string literal (type: 'char[446]')                  |
   07| +----------------------------------------------------------------------+
   08|   |   |   |   |   |   |  |  |    |    |     |     |     |     |     |
   09|   |   |   |   |   |   |  |  |    |    |     |     |     |     |     |
   10|   v   v   v   v   v   v  v  v    v    v     v     v     v     v     v
   11|+---+---------------------+----++--------------------------------------+
   12||[0]|         ...         |[99]||          after valid range           |
   13|+---+---------------------+----+|                                      |
   14||  'buf' (type: 'char[100]')   ||                                      |
   15|+------------------------------++--------------------------------------+
   16||~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~||~~~~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~~~~~|
   17|               |                                   |
   18|     +---------+---------+              +----------+----------+
   19|     |capacity: 100 bytes|              |overflow of 346 bytes|
   20|     +-------------------+              +---------------------+

   rows 01-07 and rows 11-15 are x_aligned_table_widget instances.  */

class x_aligned_table_widget : public leaf_widget
{
public:
  x_aligned_table_widget (table t,
			  const theme &theme,
			  table_dimension_sizes &col_widths)
  : m_table (std::move (t)),
    m_theme (theme),
    m_col_widths (col_widths),
    m_row_heights (t.get_size ().h),
    m_cell_sizes (m_col_widths, m_row_heights),
    m_tg (m_table, m_cell_sizes)
  {
  }

  const char *get_desc () const override
  {
    return "x_aligned_table_widget";
  }

  canvas::size_t calc_req_size () final override
  {
    /* We don't compute the size requirements;
       the parent should have done this.  */
    return m_tg.get_canvas_size ();
  }

  void paint_to_canvas (canvas &canvas) final override
  {
    m_table.paint_to_canvas (canvas,
			     get_top_left (),
			     m_tg,
			     m_theme);
  }

  const table &get_table () const { return m_table; }
  table_cell_sizes &get_cell_sizes () { return m_cell_sizes; }
  void recalc_coords ()
  {
    m_tg.recalc_coords ();
  }

private:
  table m_table;
  const theme &m_theme;
  table_dimension_sizes &m_col_widths; // Reference to shared column widths
  table_dimension_sizes m_row_heights; // Unique row heights
  table_cell_sizes m_cell_sizes;
  table_geometry m_tg;
};

/* A widget for printing arrows between the accessed region
   and the svalue, showing the direction of the access.

   For example, in:

   01| +---+---+---+---+---+---+----------+-----+-----+-----+-----+-----+-----+
   02| |[0]|[1]|[2]|[3]|[4]|[5]|   ...    |[440]|[441]|[442]|[443]|[444]|[445]|
   03| +---+---+---+---+---+---+          +-----+-----+-----+-----+-----+-----+
   04| |'L'|'o'|'r'|'e'|'m'|' '|          | 'o' | 'r' | 'u' | 'm' | '.' | NUL |
   05| +---+---+---+---+---+---+----------+-----+-----+-----+-----+-----+-----+
   06| |                  string literal (type: 'char[446]')                  |
   07| +----------------------------------------------------------------------+
   08|   |   |   |   |   |   |  |  |    |    |     |     |     |     |     |
   09|   |   |   |   |   |   |  |  |    |    |     |     |     |     |     |
   10|   v   v   v   v   v   v  v  v    v    v     v     v     v     v     v
   11|+---+---------------------+----++--------------------------------------+
   12||[0]|         ...         |[99]||          after valid range           |
   13|+---+---------------------+----+|                                      |
   14||  'buf' (type: 'char[100]')   ||                                      |
   15|+------------------------------++--------------------------------------+
   16||~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~||~~~~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~~~~~|
   17|               |                                   |
   18|     +---------+---------+              +----------+----------+
   19|     |capacity: 100 bytes|              |overflow of 346 bytes|
   20|     +-------------------+              +---------------------+

   rows 8-10 are the direction widget.  */

class direction_widget : public leaf_widget
{
public:
  direction_widget (const access_diagram_impl &dia_impl,
		    const bit_to_table_map &btm)
  : leaf_widget (),
    m_dia_impl (dia_impl),
    m_btm (btm)
  {
  }
  const char *get_desc () const override
  {
    return "direction_widget";
  }
  canvas::size_t calc_req_size () final override
  {
    /* Get our width from our siblings.  */
    return canvas::size_t (0, 3);
  }
  void paint_to_canvas (canvas &canvas) final override;

private:
  const access_diagram_impl &m_dia_impl;
  const bit_to_table_map &m_btm;
};

/* A widget for adding an x_ruler to a diagram based on table columns,
   offloading column-width calculation to shared objects, so that the ruler
   lines up with other tables in the diagram.

   For example, in:

   01| +---+---+---+---+---+---+----------+-----+-----+-----+-----+-----+-----+
   02| |[0]|[1]|[2]|[3]|[4]|[5]|   ...    |[440]|[441]|[442]|[443]|[444]|[445]|
   03| +---+---+---+---+---+---+          +-----+-----+-----+-----+-----+-----+
   04| |'L'|'o'|'r'|'e'|'m'|' '|          | 'o' | 'r' | 'u' | 'm' | '.' | NUL |
   05| +---+---+---+---+---+---+----------+-----+-----+-----+-----+-----+-----+
   06| |                  string literal (type: 'char[446]')                  |
   07| +----------------------------------------------------------------------+
   08|   |   |   |   |   |   |  |  |    |    |     |     |     |     |     |
   09|   |   |   |   |   |   |  |  |    |    |     |     |     |     |     |
   10|   v   v   v   v   v   v  v  v    v    v     v     v     v     v     v
   11|+---+---------------------+----++--------------------------------------+
   12||[0]|         ...         |[99]||          after valid range           |
   13|+---+---------------------+----+|                                      |
   14||  'buf' (type: 'char[100]')   ||                                      |
   15|+------------------------------++--------------------------------------+
   16||~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~||~~~~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~~~~~|
   17|               |                                   |
   18|     +---------+---------+              +----------+----------+
   19|     |capacity: 100 bytes|              |overflow of 346 bytes|
   20|     +-------------------+              +---------------------+

   rows 16-20 are the x_aligned_x_ruler_widget.  */

class x_aligned_x_ruler_widget : public leaf_widget
{
public:
  x_aligned_x_ruler_widget (const access_diagram_impl &dia_impl,
			    const theme &theme)
  : m_dia_impl (dia_impl),
    m_theme (theme)
  {
  }

  const char *get_desc () const override
  {
    return "x_aligned_ruler_widget";
  }

  void add_range (const table::range_t &x_range,
		  styled_string text,
		  style::id_t style_id)
  {
    m_labels.push_back (label (x_range, std::move (text), style_id));
  }

  canvas::size_t calc_req_size () final override
  {
    x_ruler r (make_x_ruler ());
    return r.get_size ();
  }

  void paint_to_canvas (canvas &canvas) final override
  {
    x_ruler r (make_x_ruler ());
    r.paint_to_canvas (canvas,
		       get_top_left (),
		       m_theme);
  }

private:
  struct label
  {
    label (const table::range_t &table_x_range,
	   styled_string text,
	   style::id_t style_id)
    : m_table_x_range (table_x_range),
      m_text (std::move (text)),
      m_style_id (style_id)
    {
    }
    table::range_t m_table_x_range;
    styled_string m_text;
    style::id_t m_style_id;
  };

  x_ruler make_x_ruler () const;

  const access_diagram_impl &m_dia_impl;
  const theme &m_theme;
  std::vector<label> m_labels;
};

/* A two-way mapping between access_ranges and table columns, for use by
   spatial_item subclasses for creating tables.
   For example when visualizing a bogus access of 'int arr[10];'
   at 'arr[10]', we might have:
   - table column 0 is "bytes 0-3" (for arr[0])
   - table column 1 is "bytes 4-35" (for arr[1] through arr[8])
   - table column 2 is "bytes 36-39 (for arr[9])
   - table column 3 is blank to emphasize a hard boundary between
     valid/invalid accesses.
   - table column 4 is "bytes 40-44" (for arr[10])

   We store this as a pair of maps from region_offset to table x; in
   the abvove example:

     region offset          table_x  prev_table_x
     bit 0 (aka byte 0)     0        (none)
     bit 32 (aka byte 4)    1        0
     bit 288 (aka byte 36)  2        1
     bit 320 (aka byte 40)  4        2
     bit 352 (aka byte 44)  (none)   (none)

     so that e.g given the half-open byte range [0, 40)
     we can determine the closed range of table x [0, 2].  */

class bit_to_table_map
{
public:
  /* Populate m_table_x_for_bit and m_bit_for_table_x.  */
  void populate (const boundaries &boundaries,
		 region_model_manager &mgr,
		 logger *logger)
  {
    LOG_SCOPE (logger);

    int table_x = 0;
    std::vector <region_offset> vec_boundaries (boundaries.begin (),
						boundaries.end ());

    /* Sort into an order that makes sense.  */
    std::sort (vec_boundaries.begin (),
	       vec_boundaries.end ());

    if (logger)
      {
	logger->log ("vec_boundaries");
	logger->inc_indent ();
	for (unsigned idx = 0; idx < vec_boundaries.size (); idx++)
	  {
	    logger->start_log_line ();
	    logger->log_partial ("idx: %i: ", idx);
	    vec_boundaries[idx].dump_to_pp (logger->get_printer (), true);
	    logger->end_log_line ();
	  }
	logger->dec_indent ();
      }

    for (size_t idx = 0; idx < vec_boundaries.size (); idx++)
      {
	const region_offset &offset = vec_boundaries[idx];
	if (idx > 0 && (idx + 1) < vec_boundaries.size ())
	  {
	    if (boundaries.get_kind (offset) == boundaries::kind::HARD)
	      table_x += 1;
	  }
	m_table_x_for_offset[offset] = table_x;
	if ((idx + 1) < vec_boundaries.size ())
	  {
	    const region_offset &next_offset = vec_boundaries[idx + 1];
	    m_table_x_for_prev_offset[next_offset] = table_x;
	    m_range_for_table_x[table_x]
	      = access_range (offset, next_offset, mgr);
	  }
	table_x += 1;
      }
    m_num_columns = table_x - 1;

    if (logger)
      log (*logger);
  }

  unsigned get_num_columns () const
  {
    return m_num_columns;
  }

  table::range_t get_table_x_for_range (const access_range &range) const
  {
    return table::range_t (get_table_x_for_offset (range.m_start),
			   get_table_x_for_prev_offset (range.m_next) + 1);
  }

  table::rect_t get_table_rect (const access_range &range,
				const int table_y, const int table_h) const
  {
    const table::range_t x_range (get_table_x_for_range (range));
    return table::rect_t (table::coord_t (x_range.start, table_y),
			  table::size_t (x_range.get_size (), table_h));
  }

  table::rect_t get_table_rect (const region *base_reg,
				const bit_range &bits,
				const int table_y, const int table_h) const
  {
    const access_range range (base_reg, bits);
    return get_table_rect (range, table_y, table_h);
  }

  table::rect_t get_table_rect (const region *base_reg,
				const byte_range &bytes,
				const int table_y, const int table_h) const
  {
    return get_table_rect (base_reg, bytes.as_bit_range (), table_y, table_h);
  }

  bool maybe_get_access_range_for_table_x (int table_x,
					   access_range *out) const
  {
    auto slot = m_range_for_table_x.find (table_x);
    if (slot == m_range_for_table_x.end ())
      return false;
    *out = slot->second;
    return true;
  }

  void log (logger &logger) const
  {
    logger.log ("table columns");
    logger.inc_indent ();
    for (unsigned table_x = 0; table_x < get_num_columns (); table_x++)
      {
	logger.start_log_line ();
	logger.log_partial ("table_x: %i", table_x);
	access_range range_for_column (NULL, bit_range (0, 0));
	if (maybe_get_access_range_for_table_x (table_x, &range_for_column))
	  {
	    logger.log_partial (": range: ");
	    range_for_column.dump_to_pp (logger.get_printer (), true);
	  }
	logger.end_log_line ();
      }
    logger.dec_indent ();
  }

  int get_table_x_for_offset (region_offset offset) const
  {
    auto slot = m_table_x_for_offset.find (offset);

    /* If this fails, then we probably failed to fully populate m_boundaries
       in find_boundaries.  */
    gcc_assert (slot != m_table_x_for_offset.end ());

    return slot->second;
  }

private:
  int get_table_x_for_prev_offset (region_offset offset) const
  {
    auto slot = m_table_x_for_prev_offset.find (offset);

    /* If this fails, then we probably failed to fully populate m_boundaries
       in find_boundaries.  */
    gcc_assert (slot != m_table_x_for_prev_offset.end ());

    return slot->second;
  }

  std::map<region_offset, int> m_table_x_for_offset;
  std::map<region_offset, int> m_table_x_for_prev_offset;
  std::map<int, access_range> m_range_for_table_x;
  unsigned m_num_columns;
};

/* Base class for something in the diagram that participates
   in two steps of diagram creation:
   (a) populating a boundaries instance with the boundaries of interest
   (b) creating a table instance for itself.

   Offsets in the boundaries are all expressed relative to the base
   region of the access_operation.  */

class spatial_item
{
public:
  virtual ~spatial_item () {}
  virtual void add_boundaries (boundaries &out, logger *) const = 0;

  virtual table make_table (const bit_to_table_map &btm,
			    style_manager &sm) const = 0;
};

/* A spatial_item that involves showing an svalue at a particular offset.  */

class svalue_spatial_item : public spatial_item
{
public:
  enum class kind
  {
     WRITTEN,
     EXISTING
  };
protected:
  svalue_spatial_item (const svalue &sval,
		       access_range bits,
		       enum kind kind)
  : m_sval (sval), m_bits (bits), m_kind (kind)
  {
  }

  const svalue &m_sval;
  access_range m_bits;
  enum kind m_kind;
};

static std::unique_ptr<spatial_item>
make_existing_svalue_spatial_item (const svalue *sval,
				   const access_range &bits,
				   const theme &theme);

class compound_svalue_spatial_item : public svalue_spatial_item
{
public:
  compound_svalue_spatial_item (const compound_svalue &sval,
				const access_range &bits,
				enum kind kind,
				const theme &theme)
  : svalue_spatial_item (sval, bits, kind),
    m_compound_sval (sval)
  {
    const binding_map &map = m_compound_sval.get_map ();
    auto_vec <const binding_key *> binding_keys;
    for (auto iter : map)
      {
	const binding_key *key = iter.first;
	const svalue *bound_sval = iter.second;
	if (const concrete_binding *concrete_key
	      = key->dyn_cast_concrete_binding ())
	  {
	    access_range range (nullptr,
				concrete_key->get_bit_range ());
	    if (std::unique_ptr<spatial_item> child
		  = make_existing_svalue_spatial_item (bound_sval,
						       range,
						       theme))
	      m_children.push_back (std::move (child));
	  }
      }
  }

  void add_boundaries (boundaries &out, logger *logger) const final override
  {
    LOG_SCOPE (logger);
    for (auto &iter : m_children)
      iter->add_boundaries (out, logger);
  }

  table make_table (const bit_to_table_map &btm,
		    style_manager &sm) const final override
  {
    std::vector<table> child_tables;
    int max_rows = 0;
    for (auto &iter : m_children)
      {
	table child_table (iter->make_table (btm, sm));
	max_rows = MAX (max_rows, child_table.get_size ().h);
	child_tables.push_back (std::move (child_table));
      }
    table t (table::size_t (btm.get_num_columns (), max_rows));
    for (auto &&child_table : child_tables)
      t.add_other_table (std::move (child_table),
			 table::coord_t (0, 0));
    return t;
  }

private:
  const compound_svalue &m_compound_sval;
  std::vector<std::unique_ptr<spatial_item>> m_children;
};

/* Loop through the TABLE_X_RANGE columns of T, adding
   cells containing "..." in any unoccupied ranges of table cell.  */

static void
add_ellipsis_to_gaps (table &t,
		      style_manager &sm,
		      const table::range_t &table_x_range,
		      const table::range_t &table_y_range)
{
  int table_x = table_x_range.get_min ();
  while (table_x < table_x_range.get_next ())
    {
      /* Find a run of unoccupied table cells.  */
      const int start_table_x = table_x;
      while (table_x < table_x_range.get_next ()
	     && !t.get_placement_at (table::coord_t (table_x,
						     table_y_range.get_min ())))
	table_x++;
      const table::range_t unoccupied_x_range (start_table_x, table_x);
      if (unoccupied_x_range.get_size () > 0)
	t.set_cell_span (table::rect_t (unoccupied_x_range, table_y_range),
			 styled_string (sm, "..."));
      /* Skip occupied table cells.  */
      while (table_x < table_x_range.get_next ()
	     && t.get_placement_at (table::coord_t (table_x,
						    table_y_range.get_min ())))
	table_x++;
    }
}

/* Subclass of spatial_item for visualizing the region of memory
   that's valid to access relative to the base region of region accessed in
   the operation.  */

class valid_region_spatial_item : public spatial_item
{
public:
  valid_region_spatial_item (const access_operation &op,
			     diagnostic_event_id_t region_creation_event_id,
			     const theme &theme)
  : m_op (op),
    m_region_creation_event_id (region_creation_event_id),
    m_boundaries (nullptr),
    m_existing_sval (op.m_model.get_store_value (op.m_base_region, nullptr)),
    m_existing_sval_spatial_item
      (make_existing_svalue_spatial_item (m_existing_sval,
					  op.get_valid_bits (),
					  theme))
  {
  }

  void add_boundaries (boundaries &out, logger *logger) const final override
  {
    LOG_SCOPE (logger);
    m_boundaries = &out;
    access_range valid_bits = m_op.get_valid_bits ();
    if (logger)
      {
	logger->start_log_line ();
	logger->log_partial ("valid bits: ");
	valid_bits.dump_to_pp (logger->get_printer (), true);
	logger->end_log_line ();
      }
    out.add (valid_bits, boundaries::kind::HARD);

    if (m_existing_sval_spatial_item)
      {
	if (logger)
	  {
	    logger->start_log_line ();
	    logger->log_partial ("existing svalue: ");
	    m_existing_sval->dump_to_pp (logger->get_printer (), true);
	    logger->end_log_line ();
	  }
	m_existing_sval_spatial_item->add_boundaries (out, logger);
      }

    /* Support for showing first and final element in array types.  */
    if (tree base_type = m_op.m_base_region->get_type ())
      if (TREE_CODE (base_type) == ARRAY_TYPE)
	{
	  if (logger)
	    logger->log ("showing first and final element in array type");
	  region_model_manager *mgr = m_op.m_model.get_manager ();
	  tree domain = TYPE_DOMAIN (base_type);
	  if (domain && TYPE_MIN_VALUE (domain) && TYPE_MAX_VALUE (domain))
	    {
	      const svalue *min_idx_sval
		= mgr->get_or_create_constant_svalue (TYPE_MIN_VALUE (domain));
	      const svalue *max_idx_sval
		= mgr->get_or_create_constant_svalue (TYPE_MAX_VALUE (domain));
	      const region *min_element =
		mgr->get_element_region (m_op.m_base_region,
					 TREE_TYPE (base_type),
					 min_idx_sval);
	      out.add (*min_element, mgr, boundaries::kind::SOFT);
	      const region *max_element =
		mgr->get_element_region (m_op.m_base_region,
					 TREE_TYPE (base_type),
					 max_idx_sval);
	      out.add (*max_element, mgr, boundaries::kind::SOFT);
	    }
	}
  }

  /* Subroutine of make_table when base region has ARRAY_TYPE.  */
  void add_array_elements_to_table (table &t,
				    const bit_to_table_map &btm,
				    style_manager &sm) const
  {
    tree base_type = m_op.m_base_region->get_type ();
    gcc_assert (TREE_CODE (base_type) == ARRAY_TYPE);
    gcc_assert (m_boundaries != nullptr);

    tree domain = TYPE_DOMAIN (base_type);
    if (!(domain && TYPE_MIN_VALUE (domain) && TYPE_MAX_VALUE (domain)))
      return;

    const int table_y = 0;
    const int table_h = 1;
    const table::range_t table_y_range (table_y, table_y + table_h);

    t.add_row ();

    const table::range_t min_x_range
      = maybe_add_array_index_to_table (t, btm, sm, table_y_range,
					TYPE_MIN_VALUE (domain));
    const table::range_t max_x_range
      = maybe_add_array_index_to_table (t, btm, sm, table_y_range,
					TYPE_MAX_VALUE (domain));

    if (TREE_TYPE (base_type) == char_type_node)
      {
	/* For a char array,: if there are any hard boundaries in
	   m_boundaries that are *within* the valid region,
	   then show those index values.  */
	std::vector<region_offset> hard_boundaries
	  = m_boundaries->get_hard_boundaries_in_range
	      (tree_to_shwi (TYPE_MIN_VALUE (domain)),
	       tree_to_shwi (TYPE_MAX_VALUE (domain)));
	for (auto &offset : hard_boundaries)
	  {
	    const int table_x = btm.get_table_x_for_offset (offset);
	    if (!offset.concrete_p ())
	      continue;
	    byte_offset_t byte;
	    if (!offset.get_concrete_byte_offset (&byte))
	      continue;
	    table::range_t table_x_range (table_x, table_x + 1);
	    t.maybe_set_cell_span (table::rect_t (table_x_range,
						  table_y_range),
				   fmt_styled_string (sm, "[%wi]",
						      byte.to_shwi ()));
	  }
      }

    add_ellipsis_to_gaps (t, sm,
			  table::range_t (min_x_range.get_next (),
					  max_x_range.get_min ()),
			  table_y_range);
  }

  table::range_t
  maybe_add_array_index_to_table (table &t,
				  const bit_to_table_map &btm,
				  style_manager &sm,
				  const table::range_t table_y_range,
				  tree idx_cst) const
  {
    region_model_manager * const mgr = m_op.get_manager ();
    tree base_type = m_op.m_base_region->get_type ();
    const svalue *idx_sval
      = mgr->get_or_create_constant_svalue (idx_cst);
    const region *element_reg = mgr->get_element_region (m_op.m_base_region,
							 TREE_TYPE (base_type),
							 idx_sval);
    const access_range element_range (*element_reg, mgr);
    const table::range_t element_x_range
      = btm.get_table_x_for_range (element_range);

    t.maybe_set_cell_span (table::rect_t (element_x_range,
					  table_y_range),
			   fmt_styled_string (sm, "[%E]", idx_cst));

    return element_x_range;
  }

  table make_table (const bit_to_table_map &btm,
		    style_manager &sm) const final override
  {
    table t (table::size_t (btm.get_num_columns (), 0));

    if (tree base_type = m_op.m_base_region->get_type ())
      if (TREE_CODE (base_type) == ARRAY_TYPE)
	add_array_elements_to_table (t, btm, sm);

    /* Make use of m_existing_sval_spatial_item, if any.  */
    if (m_existing_sval_spatial_item)
      {
	table table_for_existing
	  = m_existing_sval_spatial_item->make_table (btm, sm);
	const int table_y = t.add_rows (table_for_existing.get_size ().h);
	t.add_other_table (std::move (table_for_existing),
			   table::coord_t (0, table_y));
      }

    access_range valid_bits = m_op.get_valid_bits ();
    const int table_y = t.add_row ();
    const int table_h = 1;
    table::rect_t rect = btm.get_table_rect (valid_bits, table_y, table_h);
    styled_string s;
    switch (m_op.m_base_region->get_kind ())
      {
      default:
	s = styled_string (sm, _("region"));
	break;
      case RK_DECL:
	{
	  const decl_region *decl_reg
	    = as_a <const decl_region *> (m_op.m_base_region);
	  tree decl = decl_reg->get_decl ();
	  s = fmt_styled_string (sm, "%qE (type: %qT)",
				 decl,
				 TREE_TYPE (decl));
	}
	break;
      case RK_HEAP_ALLOCATED:
	{
	  if (m_region_creation_event_id.known_p ())
	    s = fmt_styled_string (sm, _("buffer allocated on heap at %@"),
				   &m_region_creation_event_id);
	  else
	    s = styled_string (sm, _("heap-allocated buffer"));
	}
	break;
      case RK_ALLOCA:
	{
	  if (m_region_creation_event_id.known_p ())
	    s = fmt_styled_string (sm, _("buffer allocated on stack at %@"),
				   &m_region_creation_event_id);
	  else
	    s = styled_string (sm, _("stack-allocated buffer"));
	}
	break;
      case RK_STRING:
	{
	  const string_region *string_reg
	    = as_a <const string_region *> (m_op.m_base_region);
	  tree string_cst = string_reg->get_string_cst ();
	  s = fmt_styled_string (sm, _("string literal (type: %qT)"),
				 TREE_TYPE (string_cst));
	}
	break;
      }
    t.set_cell_span (rect, std::move (s));

    return t;
  }

private:
  const access_operation &m_op;
  diagnostic_event_id_t m_region_creation_event_id;
  mutable const boundaries *m_boundaries;
  const svalue *m_existing_sval;
  std::unique_ptr<spatial_item> m_existing_sval_spatial_item;
};

/* Subclass of spatial_item for visualizing the region of memory
   that's actually accessed by the read or write, for reads and
   for write cases where we don't know the svalue written.  */

class accessed_region_spatial_item : public spatial_item
{
public:
  accessed_region_spatial_item (const access_operation &op) : m_op (op) {}

  void add_boundaries (boundaries &out, logger *logger) const final override
  {
    LOG_SCOPE (logger);
    access_range actual_bits = m_op.get_actual_bits ();
    if (logger)
      {
	logger->start_log_line ();
	logger->log_partial ("actual bits: ");
	actual_bits.dump_to_pp (logger->get_printer (), true);
	logger->end_log_line ();
      }
    out.add (actual_bits, boundaries::kind::HARD);
  }

  table make_table (const bit_to_table_map &btm,
		    style_manager &sm) const final override
  {
    table t (table::size_t (btm.get_num_columns (), 1));

    access_range actual_bits = m_op.get_actual_bits ();
    const int table_y = 0;
    const int table_h = 1;
    table::rect_t rect = btm.get_table_rect (actual_bits, table_y, table_h);
    t.set_cell_span (rect, styled_string (get_label_string (sm)));

    return t;
  }

private:
  styled_string get_label_string (style_manager &sm) const
  {
    const access_range accessed_bits (m_op.get_actual_bits ());
    return get_access_size_str (sm,
				m_op,
				accessed_bits,
				m_op.m_reg.get_type ());
  }

  const access_operation &m_op;
};

/* Subclass of spatial_item for when we know the svalue being written
   to the accessed region.
   Can be subclassed to give visualizations of specific kinds of svalue.  */

class written_svalue_spatial_item : public spatial_item
{
public:
  written_svalue_spatial_item (const access_operation &op,
		       const svalue &sval,
		       access_range actual_bits)
  : m_op (op), m_sval (sval), m_actual_bits (actual_bits)
  {}

  void add_boundaries (boundaries &out, logger *logger) const override
  {
    LOG_SCOPE (logger);
    out.add (m_actual_bits, boundaries::kind::HARD);
  }

  table make_table (const bit_to_table_map &btm,
		    style_manager &sm) const override
  {
    table t (table::size_t (btm.get_num_columns (), 0));

    const int table_y = t.add_row ();
    const int table_h = 1;
    table::rect_t rect = btm.get_table_rect (m_actual_bits, table_y, table_h);
    t.set_cell_span (rect, styled_string (get_label_string (sm)));
    return t;
  }

protected:
  styled_string get_label_string (style_manager &sm) const
  {
    tree rep_tree = m_op.m_model.get_representative_tree (&m_sval);
    if (rep_tree)
      {
	if (TREE_CODE (rep_tree) == SSA_NAME)
	  if (tree var = SSA_NAME_VAR (rep_tree))
	    rep_tree = var;
	switch (TREE_CODE (rep_tree))
	  {
	  default:
	    break;
	  case INTEGER_CST:
	    return fmt_styled_string (sm, _("write of %<(%T) %E%>"),
				      TREE_TYPE (rep_tree),
				      rep_tree);

	  case PARM_DECL:
	  case VAR_DECL:
	    return fmt_styled_string (sm, _("write from %qE (type: %qT)"),
				      rep_tree,
				      TREE_TYPE (rep_tree));
	    break;
	  }
	}

    const access_range accessed_bits (m_op.get_actual_bits ());
    return get_access_size_str (sm,
				m_op,
				accessed_bits,
				m_sval.get_type ());
  }

  const access_operation &m_op;
  const svalue &m_sval;
  access_range m_actual_bits;
};

/* Subclass of svalue_spatial_item for initial_svalue of a string_region
   i.e. for string literals.

   There are three cases:
   (a) for long strings, show just the head and tail of the string,
   with an ellipsis:
     +---+---+---+---+---+---+----------+-----+-----+-----+-----+-----+-----+
     |[0]|[1]|[2]|[3]|[4]|[5]|          |[440]|[441]|[442]|[443]|[444]|[445]|
     +---+---+---+---+---+---+   ...    +-----+-----+-----+-----+-----+-----+
     |‘L’|‘o’|‘r’|‘e’|‘m’|‘ ’|          | ‘o’ | ‘r’ | ‘u’ | ‘m’ | ‘.’ | NUL |
     +---+---+---+---+---+---+----------+-----+-----+-----+-----+-----+-----+
     |                  string literal (type: ‘char[446]’)                  |
     +----------------------------------------------------------------------+
   (b) For sufficiently short strings, show the full string:
     +----------+---------+---------+---------+---------+ +-----------------+
     |   [0]    |   [1]   |   [2]   |   [3]   |   [4]   | |       [5]       |
     +----------+---------+---------+---------+---------+ +-----------------+
     |   ‘h’    |   ‘e’   |   ‘l’   |   ‘l’   |   ‘o’   | |       NUL       |
     +----------+---------+---------+---------+---------+-+-----------------+
     |                   string literal (type: ‘char[6]’)                   |
     +----------------------------------------------------------------------+
   (c) for non-ASCII strings that are short enough to show the full string,
   show how unicode code points of the bytes decoded as UTF-8:
     +-----+-----+-----+----+----++----+----+----+----+----+----+----+------+
     | [0] | [1] | [2] |[3] |[4] ||[5] |[6] |[7] |[8] |[9] |[10]|[11]| [12] |
     +-----+-----+-----+----+----++----+----+----+----+----+----+----+------+
     |0xe6 |0x96 |0x87 |0xe5|0xad||0x97|0xe5|0x8c|0x96|0xe3|0x81|0x91| 0x00 |
     +-----+-----+-----+----+----++----+----+----+----+----+----+----+------+
     |     U+6587      |    U+5b57     |    U+5316    |    U+3051    |U+0000|
     +-----------------+---------------+--------------+--------------+------+
     |                  string literal (type: ‘char[13]’)                   |
     +----------------------------------------------------------------------+
   and show the characters themselves if unicode is supported and they are not
   control characters:
     ┌─────┬─────┬─────┬────┬────┐┌────┬────┬────┬────┬────┬────┬────┬──────┐
     │ [0] │ [1] │ [2] │[3] │[4] ││[5] │[6] │[7] │[8] │[9] │[10]│[11]│ [12] │
     ├─────┼─────┼─────┼────┼────┤├────┼────┼────┼────┼────┼────┼────┼──────┤
     │0xe6 │0x96 │0x87 │0xe5│0xad││0x97│0xe5│0x8c│0x96│0xe3│0x81│0x91│ 0x00 │
     ├─────┴─────┴─────┼────┴────┴┴────┼────┴────┴────┼────┴────┴────┼──────┤
     │     U+6587      │    U+5b57     │    U+5316    │    U+3051    │U+0000│
     ├─────────────────┼───────────────┼──────────────┼──────────────┼──────┤
     │       文        │      字       │      化      │      け      │ NUL  │
     ├─────────────────┴───────────────┴──────────────┴──────────────┴──────┤
     │                  string literal (type: ‘char[13]’)                   │
     └──────────────────────────────────────────────────────────────────────┘
*/

class string_literal_spatial_item : public svalue_spatial_item
{
public:
  string_literal_spatial_item (const svalue &sval,
			       access_range actual_bits,
			       const string_region &string_reg,
			       const theme &theme,
			       enum kind kind)
  : svalue_spatial_item (sval, actual_bits, kind),
    m_string_reg (string_reg),
    m_theme (theme),
    m_ellipsis_threshold (param_analyzer_text_art_string_ellipsis_threshold),
    m_ellipsis_head_len (param_analyzer_text_art_string_ellipsis_head_len),
    m_ellipsis_tail_len (param_analyzer_text_art_string_ellipsis_tail_len),
    m_show_full_string (calc_show_full_string ()),
    m_show_utf8 (m_show_full_string && !pure_ascii_p ())
  {
  }

  void add_boundaries (boundaries &out, logger *logger) const override
  {
    LOG_SCOPE (logger);
    out.add (m_bits, m_kind == svalue_spatial_item::kind::WRITTEN
	     ? boundaries::kind::HARD
	     : boundaries::kind::SOFT);

    tree string_cst = get_string_cst ();
    /* TREE_STRING_LENGTH is sizeof, not strlen.  */
    if (m_show_full_string)
      out.add_all_bytes_in_range (m_bits);
    else
      {
	byte_range bytes (0, 0);
	bool valid = m_bits.as_concrete_byte_range (&bytes);
	gcc_assert (valid);
	byte_range head_of_string (bytes.get_start_byte_offset (),
				   m_ellipsis_head_len);
	out.add_all_bytes_in_range (head_of_string);
	byte_range tail_of_string
	  ((bytes.get_start_byte_offset ()
	    + TREE_STRING_LENGTH (string_cst)
	    - m_ellipsis_tail_len),
	   m_ellipsis_tail_len);
	out.add_all_bytes_in_range (tail_of_string);
	/* Adding the above pair of ranges will also effectively add
	   the boundaries of the range of ellipsized chars, as they're
	   exactly in between head_of_string and tail_of_string.  */
      }
  }

  table make_table (const bit_to_table_map &btm,
		    style_manager &sm) const override
  {
    table t (table::size_t (btm.get_num_columns (), 0));

    const int byte_idx_table_y = (m_kind == svalue_spatial_item::kind::WRITTEN
				  ? t.add_row ()
				  : -1);
    const int byte_val_table_y = t.add_row ();

    byte_range bytes (0, 0);
    bool valid = m_bits.as_concrete_byte_range (&bytes);
    gcc_assert (valid);
    tree string_cst = get_string_cst ();
    if (m_show_full_string)
      {
       for (byte_offset_t byte_idx_within_cluster
	      = bytes.get_start_byte_offset ();
	    byte_idx_within_cluster < bytes.get_next_byte_offset ();
	    byte_idx_within_cluster = byte_idx_within_cluster + 1)
	 add_column_for_byte
	   (t, btm, sm, byte_idx_within_cluster,
	    byte_idx_within_cluster - bytes.get_start_byte_offset (),
	    byte_idx_table_y, byte_val_table_y);

       if (m_show_utf8)
	 {
	   const bool show_unichars = m_theme.unicode_p ();
	   const int utf8_code_point_table_y = t.add_row ();
	   int utf8_character_table_y;
	   if (show_unichars)
	     utf8_character_table_y = t.add_row ();

	   /* We don't actually want the display widths here, but
	      it's an easy way to decode UTF-8.  */
	   cpp_char_column_policy policy (8, cpp_wcwidth);
	   cpp_display_width_computation dw (TREE_STRING_POINTER (string_cst),
					     TREE_STRING_LENGTH (string_cst),
					     policy);
	   while (!dw.done ())
	     {
	       cpp_decoded_char decoded_char;
	       dw.process_next_codepoint (&decoded_char);

	       if (!decoded_char.m_valid_ch)
		 continue;
	       size_t start_byte_idx
		 = decoded_char.m_start_byte - TREE_STRING_POINTER (string_cst);
	       byte_size_t size_in_bytes
		 = decoded_char.m_next_byte - decoded_char.m_start_byte;
	       byte_range cluster_bytes_for_codepoint
		 (start_byte_idx + bytes.get_start_byte_offset (),
		  size_in_bytes);

	       const table::rect_t code_point_table_rect
		 = btm.get_table_rect (&m_string_reg,
				       cluster_bytes_for_codepoint,
				       utf8_code_point_table_y, 1);
	       char buf[100];
	       sprintf (buf, "U+%04x", decoded_char.m_ch);
	       t.set_cell_span (code_point_table_rect,
				styled_string (sm, buf));

	       if (show_unichars)
		 {
		   const table::rect_t character_table_rect
		     = btm.get_table_rect (&m_string_reg,
					   cluster_bytes_for_codepoint,
					   utf8_character_table_y, 1);
		   if (cpp_is_printable_char (decoded_char.m_ch))
		     t.set_cell_span (character_table_rect,
				      styled_string (decoded_char.m_ch));
		   else if (decoded_char.m_ch == 0)
		     t.set_cell_span (character_table_rect,
				      styled_string (sm, "NUL"));
		   else
		     t.set_cell_span (character_table_rect,
				      styled_string (sm, ""));
		 }
	     }
	 }
      }
    else
      {
	/* Head of string.  */
	for (int byte_idx = 0; byte_idx < m_ellipsis_head_len; byte_idx++)
	  add_column_for_byte (t, btm, sm,
			       byte_idx + bytes.get_start_byte_offset (),
			       byte_idx,
			       byte_idx_table_y, byte_val_table_y);

	/* Ellipsis.  */
	const byte_range ellipsis_bytes
	  (m_ellipsis_head_len + bytes.get_start_byte_offset (),
	   TREE_STRING_LENGTH (string_cst)
	   - (m_ellipsis_head_len + m_ellipsis_tail_len));
	const table::rect_t table_rect
	  = ((byte_idx_table_y != -1)
	     ? btm.get_table_rect (&m_string_reg, ellipsis_bytes,
				   byte_idx_table_y, 2)
	     : btm.get_table_rect (&m_string_reg, ellipsis_bytes,
				   byte_val_table_y, 1));
	t.set_cell_span(table_rect, styled_string (sm, "..."));

	/* Tail of string.  */
	for (int byte_idx
	       = (TREE_STRING_LENGTH (string_cst) - m_ellipsis_tail_len);
	     byte_idx < TREE_STRING_LENGTH (string_cst);
	     byte_idx++)
	  add_column_for_byte (t, btm, sm,
			       byte_idx + bytes.get_start_byte_offset (),
			       byte_idx,
			       byte_idx_table_y, byte_val_table_y);
      }

    if (m_kind == svalue_spatial_item::kind::WRITTEN)
      {
	const int summary_table_y = t.add_row ();
	t.set_cell_span (btm.get_table_rect (&m_string_reg, bytes,
					     summary_table_y, 1),
			 fmt_styled_string (sm,
					    _("string literal (type: %qT)"),
					    TREE_TYPE (string_cst)));
      }

    return t;
  }

  tree get_string_cst () const { return m_string_reg.get_string_cst (); }

private:
  bool calc_show_full_string () const
  {
    tree string_cst = get_string_cst ();
    if (TREE_STRING_LENGTH (string_cst) < m_ellipsis_threshold)
      return true;
    if (TREE_STRING_LENGTH (string_cst) <
	(m_ellipsis_head_len + m_ellipsis_tail_len))
      return true;
    return false;
  }

  bool pure_ascii_p () const
  {
    tree string_cst = get_string_cst ();
    for (unsigned byte_idx = 0;
	 byte_idx < (unsigned) TREE_STRING_LENGTH (string_cst);
	 byte_idx++)
      {
	unsigned char ch = TREE_STRING_POINTER (string_cst)[byte_idx];
	if (ch >= 0x80)
	  return false;
      }
    return true;
  }

  void add_column_for_byte (table &t, const bit_to_table_map &btm,
			    style_manager &sm,
			    const byte_offset_t byte_idx_within_cluster,
			    const byte_offset_t byte_idx_within_string,
			    const int byte_idx_table_y,
			    const int byte_val_table_y) const
  {
    tree string_cst = get_string_cst ();
    gcc_assert (byte_idx_within_string >= 0);
    gcc_assert (byte_idx_within_string < TREE_STRING_LENGTH (string_cst));

    const byte_range bytes (byte_idx_within_cluster, 1);
    if (byte_idx_table_y != -1)
      {
	const table::rect_t idx_table_rect
	  = btm.get_table_rect (&m_string_reg, bytes, byte_idx_table_y, 1);
	t.set_cell_span (idx_table_rect,
			 fmt_styled_string (sm, "[%wu]",
					    byte_idx_within_string.ulow ()));
      }

    char byte_val
      = TREE_STRING_POINTER (string_cst)[byte_idx_within_string.ulow ()];
    const table::rect_t val_table_rect
      = btm.get_table_rect (&m_string_reg, bytes, byte_val_table_y, 1);
    table_cell_content content (make_cell_content_for_byte (sm, byte_val));
    t.set_cell_span (val_table_rect, std::move (content));
  }

  table_cell_content make_cell_content_for_byte (style_manager &sm,
						 unsigned char byte_val) const
  {
    if (!m_show_utf8)
       {
	if (byte_val == '\0')
	  return styled_string (sm, "NUL");
	else if (byte_val < 0x80)
	  if (ISPRINT (byte_val))
	    return fmt_styled_string (sm, "%qc", byte_val);
       }
    char buf[100];
    sprintf (buf, "0x%02x", byte_val);
    return styled_string (sm, buf);
  }

  const string_region &m_string_reg;
  const theme &m_theme;
  const int m_ellipsis_threshold;
  const int m_ellipsis_head_len;
  const int m_ellipsis_tail_len;
  const bool m_show_full_string;
  const bool m_show_utf8;
};

static std::unique_ptr<spatial_item>
make_written_svalue_spatial_item (const access_operation &op,
				  const svalue &sval,
				  access_range actual_bits,
				  const theme &theme)
{
  if (const initial_svalue *initial_sval = sval.dyn_cast_initial_svalue ())
    if (const string_region *string_reg
	= initial_sval->get_region ()->dyn_cast_string_region ())
      return make_unique <string_literal_spatial_item>
	(sval, actual_bits,
	 *string_reg, theme,
	 svalue_spatial_item::kind::WRITTEN);
  return make_unique <written_svalue_spatial_item> (op, sval, actual_bits);
}

static std::unique_ptr<spatial_item>
make_existing_svalue_spatial_item (const svalue *sval,
				   const access_range &bits,
				   const theme &theme)
{
  if (!sval)
    return nullptr;

  switch (sval->get_kind ())
    {
    default:
      return nullptr;

    case SK_INITIAL:
      {
	const initial_svalue *initial_sval = (const initial_svalue *)sval;
	if (const string_region *string_reg
	    = initial_sval->get_region ()->dyn_cast_string_region ())
	  return make_unique <string_literal_spatial_item>
	    (*sval, bits,
	     *string_reg, theme,
	     svalue_spatial_item::kind::EXISTING);
	return nullptr;
      }

    case SK_COMPOUND:
      return make_unique<compound_svalue_spatial_item>
	(*((const compound_svalue *)sval),
	 bits,
	 svalue_spatial_item::kind::EXISTING,
	 theme);
    }
}

/* Widget subclass implementing access diagrams.  */

class access_diagram_impl : public vbox_widget
{
public:
  access_diagram_impl (const access_operation &op,
		       diagnostic_event_id_t region_creation_event_id,
		       style_manager &sm,
		       const theme &theme,
		       logger *logger)
  : m_op (op),
    m_region_creation_event_id (region_creation_event_id),
    m_sm (sm),
    m_theme (theme),
    m_logger (logger),
    m_invalid (false),
    m_valid_region_spatial_item (op, region_creation_event_id, theme),
    m_accessed_region_spatial_item (op),
    m_btm (),
    m_calc_req_size_called (false)
  {
    LOG_SCOPE (logger);

    if (logger)
      {
	access_range invalid_before_bits;
	if (op.maybe_get_invalid_before_bits (&invalid_before_bits))
	  invalid_before_bits.log ("invalid before range", *logger);
	access_range invalid_after_bits;
	if (op.maybe_get_invalid_after_bits (&invalid_after_bits))
	  invalid_after_bits.log ("invalid after range", *logger);

	if (op.m_sval_hint)
	  {
	    logger->start_log_line ();
	    logger->log_partial ("sval_hint: ");
	    op.m_sval_hint->dump_to_pp (logger->get_printer (), true);
	    logger->end_log_line ();
	  }
      }

    /* Register painting styles.  */
    {
      style valid_style (get_style_from_color_cap_name ("valid"));
      m_valid_style_id = m_sm.get_or_create_id (valid_style);

      style invalid_style (get_style_from_color_cap_name ("invalid"));
      m_invalid_style_id = m_sm.get_or_create_id (invalid_style);
    }

    if (op.m_sval_hint)
      {
	access_range actual_bits = m_op.get_actual_bits ();
	m_written_svalue_spatial_item
	  = make_written_svalue_spatial_item (m_op,
					      *op.m_sval_hint,
					      actual_bits,
					      m_theme);
      }

    /* Two passes:
       First, figure out all of the boundaries of interest.
       Then use that to build child widgets showing the regions of interest,
       with a common tabular layout.  */

    m_boundaries = find_boundaries ();
    if (logger)
      m_boundaries->log (*logger);

    /* Populate m_table_x_for_bit and m_bit_for_table_x.
       Each table column represents the range [offset, next_offset).
       We don't create a column in the table for the final offset, but we
       do populate it, so that looking at the table_x of one beyond the
       final table column gives us the upper bound offset.  */
    m_btm.populate (*m_boundaries, *m_op.get_manager (), logger);

    /* Gracefully reject cases where the boundary sorting has gone wrong
       (due to awkward combinations of symbolic values).  */
    {
      table::range_t actual_bits_x_range
	= m_btm.get_table_x_for_range (m_op.get_actual_bits ());
      if (actual_bits_x_range.get_size () <= 0)
	{
	  if (logger)
	    logger->log ("giving up: bad table columns for actual_bits");
	  m_invalid = true;
	  return;
	}
      table::range_t valid_bits_x_range
	= m_btm.get_table_x_for_range (m_op.get_valid_bits ());
      if (valid_bits_x_range.get_size () <= 0)
	{
	  if (logger)
	    logger->log ("giving up: bad table columns for valid_bits");
	  m_invalid = true;
	  return;
	}
    }

    m_col_widths
      = make_unique <table_dimension_sizes> (m_btm.get_num_columns ());

    /* Now create child widgets.  */

    if (flag_analyzer_debug_text_art)
      {
	table t_headings (make_headings_table ());
	add_aligned_child_table (std::move (t_headings));
      }

    if (m_written_svalue_spatial_item)
      {
	table t_sval (m_written_svalue_spatial_item->make_table (m_btm, m_sm));
	add_aligned_child_table (std::move (t_sval));
      }
    else
      {
	table t_accessed
	  (m_accessed_region_spatial_item.make_table (m_btm, m_sm));
	add_aligned_child_table (std::move (t_accessed));
      }

    add_direction_widget ();

    table t_valid (m_valid_region_spatial_item.make_table (m_btm, m_sm));
    add_invalid_accesses_to_region_table (t_valid);
    add_aligned_child_table (std::move (t_valid));

    add_valid_vs_invalid_ruler ();
  }

  const char *get_desc () const override
  {
    return "access_diagram_impl";
  }

  canvas::size_t calc_req_size () final override
  {
    if (m_invalid)
      return canvas::size_t (0, 0);

    /* Now compute the size requirements for the tables.  */
    for (auto iter : m_aligned_table_widgets)
      iter->get_cell_sizes ().pass_1 (iter->get_table ());
    for (auto iter : m_aligned_table_widgets)
      iter->get_cell_sizes ().pass_2 (iter->get_table ());

    adjust_to_scale();

    /* ...and relayout the tables.  */
    for (auto iter : m_aligned_table_widgets)
      iter->recalc_coords ();

    /* Populate the canvas_x per table_x.  */
    m_col_start_x.clear ();
    int iter_canvas_x = 0;
    for (auto w : m_col_widths->m_requirements)
      {
	m_col_start_x.push_back (iter_canvas_x);
	iter_canvas_x += w + 1;
      }
    m_col_start_x.push_back (iter_canvas_x);

    m_calc_req_size_called = true;

    return vbox_widget::calc_req_size ();
  }

  int get_canvas_x_for_table_x (int table_x) const
  {
    gcc_assert (m_calc_req_size_called);
    return m_col_start_x[table_x];
  }

  canvas::range_t get_canvas_x_range (const table::range_t &table_x_range) const
  {
    gcc_assert (m_calc_req_size_called);
    return canvas::range_t (get_canvas_x_for_table_x (table_x_range.start),
			    get_canvas_x_for_table_x (table_x_range.next));
  }

  const access_operation &get_op () const { return m_op; }

  style::id_t get_style_id_for_validity (bool is_valid) const
  {
    return is_valid ? m_valid_style_id : m_invalid_style_id;
  }

  const theme &get_theme () const { return m_theme; }

private:
  /* Figure out all of the boundaries of interest when visualizing ths op.  */
  std::unique_ptr<boundaries>
  find_boundaries () const
  {
    std::unique_ptr<boundaries> result
      = make_unique<boundaries> (*m_op.m_base_region, m_logger);

    m_valid_region_spatial_item.add_boundaries (*result, m_logger);
    m_accessed_region_spatial_item.add_boundaries (*result, m_logger);
    if (m_written_svalue_spatial_item)
      m_written_svalue_spatial_item->add_boundaries (*result, m_logger);

    return result;
  }

  void add_aligned_child_table (table t)
  {
    x_aligned_table_widget *w
      = new x_aligned_table_widget (std::move (t), m_theme, *m_col_widths);
    m_aligned_table_widgets.push_back (w);
    add_child (std::unique_ptr<widget> (w));
  }

  /* Create a table showing headings for use by -fanalyzer-debug-text-art, for
     example:
     +---------+-----------+-----------+---+--------------------------------+
     |   tc0   |    tc1    |    tc2    |tc3|              tc4               |
     +---------+-----------+-----------+---+--------------------------------+
     |bytes 0-3|bytes 4-35 |bytes 36-39|   |          bytes 40-43           |
     +---------+-----------+-----------+   +--------------------------------+
     which has:
     - a row showing the table column numbers, labelled "tc0", "tc1", etc
     - a row showing the memory range of each table column that has one.  */

  table make_headings_table () const
  {
    table t (table::size_t (m_btm.get_num_columns (), 2));

    for (int table_x = 0; table_x < t.get_size ().w; table_x++)
      {
	const int table_y = 0;
	t.set_cell (table::coord_t (table_x, table_y),
		    fmt_styled_string (m_sm, "tc%i", table_x));
      }
    for (int table_x = 0; table_x < t.get_size ().w; table_x++)
      {
	const int table_y = 1;
	access_range range_for_column (NULL, bit_range (0, 0));
	if (m_btm.maybe_get_access_range_for_table_x (table_x,
						      &range_for_column))
	  {
	    pretty_printer pp;
	    pp_format_decoder (&pp) = default_tree_printer;
	    range_for_column.dump_to_pp (&pp, true);
	    t.set_cell (table::coord_t (table_x, table_y),
			styled_string (m_sm, pp_formatted_text (&pp)));
	  }
      }

    return t;
  }

  void add_direction_widget ()
  {
    add_child (::make_unique<direction_widget> (*this, m_btm));
  }

  void add_invalid_accesses_to_region_table (table &t_region)
  {
    gcc_assert (t_region.get_size ().w == (int)m_btm.get_num_columns ());

    const int table_y = 0;
    const int table_h = t_region.get_size ().h;

    access_range invalid_before_bits;
    if (m_op.maybe_get_invalid_before_bits (&invalid_before_bits))
      {
	t_region.set_cell_span (m_btm.get_table_rect (invalid_before_bits,
						      table_y, table_h),
				styled_string (m_sm,
					       _("before valid range")));
      }
    access_range invalid_after_bits;
    if (m_op.maybe_get_invalid_after_bits (&invalid_after_bits))
      {
	t_region.set_cell_span (m_btm.get_table_rect (invalid_after_bits,
						      table_y, table_h),
				styled_string (m_sm,
					       _("after valid range")));
      }
  }

  void maybe_add_gap (x_aligned_x_ruler_widget *w,
		      const access_range &lower,
		      const access_range &upper) const
  {
    LOG_SCOPE (m_logger);
    if (m_logger)
      {
	lower.log ("lower", *m_logger);
	upper.log ("upper", *m_logger);
      }
    region_model_manager *mgr = m_op.get_manager ();
    const svalue &lower_next = lower.m_next.calc_symbolic_bit_offset (mgr);
    const svalue &upper_start = upper.m_start.calc_symbolic_bit_offset (mgr);
    const svalue *num_bits_gap
      = mgr->get_or_create_binop (NULL_TREE, MINUS_EXPR,
				  &upper_start, &lower_next);
    if (m_logger)
      m_logger->log ("num_bits_gap: %qs", num_bits_gap->get_desc ().get ());

    const svalue *zero = mgr->get_or_create_int_cst (NULL_TREE, 0);
    tristate ts_gt_zero = m_op.m_model.eval_condition (num_bits_gap,
						       GT_EXPR,
						       zero);
    if (ts_gt_zero.is_false ())
      {
	if (m_logger)
	  m_logger->log ("rejecting as not > 0");
	return;
      }

    bit_size_expr num_bits (*num_bits_gap);
    if (auto p = num_bits.maybe_get_formatted_str (m_sm, m_op.m_model,
						   _("%wi bit"),
						   _("%wi bits"),
						   _("%wi byte"),
						   _("%wi bytes"),
						   _("%qs bits"),
						   _("%qs bytes")))
      {
	styled_string label = std::move (*p.get ());
	w->add_range (m_btm.get_table_x_for_range
		      (access_range (lower.m_next,
				     upper.m_start,
				     *mgr)),
		      std::move (label),
		      style::id_plain);
      }
  }

  styled_string
  make_warning_string (styled_string &&text)
  {
    styled_string result;
    if (!m_theme.emojis_p ())
      return std::move (text);

    result.append (styled_string (0x26A0, /* U+26A0 WARNING SIGN.  */
				  true));
    /* U+26A0 WARNING SIGN has East_Asian_Width == Neutral, but in its
       emoji variant is printed (by vte at least) with a 2nd half
       overlapping the next char.  Hence we add two spaces here: a space
       to be covered by this overlap, plus another space of padding.  */
    result.append (styled_string (m_sm, "  "));
    result.append (std::move (text));
    return result;
  }

  /* Add a ruler child widet showing valid, invalid, and gaps.  */
  void add_valid_vs_invalid_ruler ()
  {
    LOG_SCOPE (m_logger);

    x_aligned_x_ruler_widget *w
      = new x_aligned_x_ruler_widget (*this, m_theme);

    access_range invalid_before_bits;
    if (m_op.maybe_get_invalid_before_bits (&invalid_before_bits))
      {
	if (m_logger)
	  invalid_before_bits.log ("invalid_before_bits", *m_logger);
	bit_size_expr num_before_bits
	  (invalid_before_bits.get_size (m_op.get_manager ()));
	std::unique_ptr<styled_string> label;
	if (m_op.m_dir == DIR_READ)
	  label = num_before_bits.maybe_get_formatted_str
	    (m_sm, m_op.m_model,
	     _("under-read of %wi bit"),
	     _("under-read of %wi bits"),
	     _("under-read of %wi byte"),
	     _("under-read of %wi bytes"),
	     _("under-read of %qs bits"),
	     _("under-read of %qs bytes"));
	else
	  label = num_before_bits.maybe_get_formatted_str
	    (m_sm, m_op.m_model,
	     _("underwrite of %wi bit"),
	     _("underwrite of %wi bits"),
	     _("underwrite of %wi byte"),
	     _("underwrite of %wi bytes"),
	     _("underwrite of %qs bits"),
	     _("underwrite of %qs bytes"));
	if (label)
	  w->add_range (m_btm.get_table_x_for_range (invalid_before_bits),
			make_warning_string (std::move (*label)),
			m_invalid_style_id);
      }
    else
      {
	if (m_logger)
	  m_logger->log ("no invalid_before_bits");
      }

    /* It would be nice to be able to use std::optional<access_range> here,
       but std::optional is C++17.  */
    bool got_valid_bits = false;
    access_range valid_bits (m_op.get_valid_bits ());
    bit_size_expr num_valid_bits (valid_bits.get_size (m_op.get_manager ()));
    if (m_logger)
      valid_bits.log ("valid_bits", *m_logger);

    got_valid_bits = true;
    maybe_add_gap (w, invalid_before_bits, valid_bits);

    std::unique_ptr<styled_string> label;
    if (m_op.m_dir == DIR_READ)
      label = num_valid_bits.maybe_get_formatted_str (m_sm,
						      m_op.m_model,
						      _("size: %wi bit"),
						      _("size: %wi bits"),
						      _("size: %wi byte"),
						      _("size: %wi bytes"),
						      _("size: %qs bits"),
						      _("size: %qs bytes"));
    else
      label
	= num_valid_bits.maybe_get_formatted_str (m_sm,
						  m_op.m_model,
						  _("capacity: %wi bit"),
						  _("capacity: %wi bits"),
						  _("capacity: %wi byte"),
						  _("capacity: %wi bytes"),
						  _("capacity: %qs bits"),
						  _("capacity: %qs bytes"));
    if (label)
      w->add_range (m_btm.get_table_x_for_range (m_op.get_valid_bits ()),
		    std::move (*label),
		    m_valid_style_id);

    access_range invalid_after_bits;
    if (m_op.maybe_get_invalid_after_bits (&invalid_after_bits))
      {
	if (got_valid_bits)
	  maybe_add_gap (w, valid_bits, invalid_after_bits);

	if (m_logger)
	  invalid_before_bits.log ("invalid_after_bits", *m_logger);

	bit_size_expr num_after_bits
	  (invalid_after_bits.get_size (m_op.get_manager ()));
	std::unique_ptr<styled_string> label;
	if (m_op.m_dir == DIR_READ)
	  label = num_after_bits.maybe_get_formatted_str
	    (m_sm, m_op.m_model,
	     _("over-read of %wi bit"),
	     _("over-read of %wi bits"),
	     _("over-read of %wi byte"),
	     _("over-read of %wi bytes"),
	     _("over-read of %qs bits"),
	     _("over-read of %qs bytes"));
	else
	  label = num_after_bits.maybe_get_formatted_str
	    (m_sm, m_op.m_model,
	     _("overflow of %wi bit"),
	     _("overflow of %wi bits"),
	     _("overflow of %wi byte"),
	     _("overflow of %wi bytes"),
	     _("overflow of %qs bits"),
	     _("overflow of %qs bytes"));
	if (label)
	  w->add_range (m_btm.get_table_x_for_range (invalid_after_bits),
			make_warning_string (std::move (*label)),
			m_invalid_style_id);
      }
    else
      {
	if (m_logger)
	  m_logger->log ("no invalid_after_bits");
      }

    add_child (std::unique_ptr<widget> (w));
  }

  /* Subroutine of calc_req_size.
     Try to allocate surplus canvas width to table columns to make the
     per table-column canvas widths closer to being to scale.
     See e.g.:
       https://en.wikipedia.org/wiki/Fair_item_allocation
       https://en.wikipedia.org/wiki/Mathematics_of_apportionment
  */
  void adjust_to_scale ()
  {
    LOG_SCOPE (m_logger);
    const unsigned num_columns = m_btm.get_num_columns ();
    std::vector<bit_offset_t> bit_sizes (num_columns);
    for (unsigned table_x = 0; table_x < num_columns; table_x++)
      {
	access_range range_for_column (NULL, bit_range (0, 0));
	if (m_btm.maybe_get_access_range_for_table_x (table_x,
						      &range_for_column))
	  {
	    bit_size_t size_in_bits;
	    if (!range_for_column.get_size_in_bits (&size_in_bits))
	      size_in_bits = BITS_PER_UNIT; // arbitrary non-zero value
	    gcc_assert (size_in_bits > 0);
	    bit_sizes[table_x] = size_in_bits;
	  }
	else
	  bit_sizes[table_x] = 0;
      }

    while (adjust_to_scale_once (bit_sizes))
      {
      }
  }
  bool adjust_to_scale_once (const std::vector<bit_offset_t> &bit_sizes)
  {
    LOG_SCOPE (m_logger);

    const unsigned num_columns = m_btm.get_num_columns ();

    /* Find the total canvas width currently required.
       Require one extra canvas column for the right-hand border
       of the table.  */
    int total_width = 1;
    for (unsigned table_x = 0; table_x < num_columns; table_x++)
      {
	int canvas_w = m_col_widths->m_requirements[table_x];
	gcc_assert (canvas_w >= 0);
	total_width += canvas_w + 1;
      }

    const int max_width = param_analyzer_text_art_ideal_canvas_width;
    if (total_width >= max_width)
      {
	if (m_logger)
	  m_logger->log ("bailing out: total_width=%i ,>= max_width (%i)\n",
			 total_width, max_width);
	return false;
      }

    const int fixed_point = 1024;
    std::vector<bit_offset_t> canvas_w_per_bit (num_columns);
    for (unsigned table_x = 0; table_x < num_columns; table_x++)
      {
	bit_offset_t bit_size = bit_sizes[table_x];
	if (bit_size > 0)
	  canvas_w_per_bit[table_x]
	    = (m_col_widths->m_requirements[table_x] * fixed_point) / bit_size;
	else
	  canvas_w_per_bit[table_x] = INT_MAX;
      }

    /* Find the min canvas per bit, and give an extra canvas column to
       the table column that has least.  */
    size_t min_idx = std::distance (canvas_w_per_bit.begin (),
				    std::min_element (canvas_w_per_bit.begin (),
						      canvas_w_per_bit.end ()));
    m_col_widths->m_requirements[min_idx] += 1;
    if (m_logger)
      m_logger->log ("adding 1 canvas_w to column %i\n", (int)min_idx);

    return true; // keep going
  }

  const access_operation &m_op;
  diagnostic_event_id_t m_region_creation_event_id;
  style_manager &m_sm;
  const theme &m_theme;
  logger *m_logger;
  /* In lieu of being able to throw exceptions, a flag to mark this object
     as "invalid".  */
  bool m_invalid;

  style::id_t m_valid_style_id;
  style::id_t m_invalid_style_id;

  valid_region_spatial_item m_valid_region_spatial_item;
  accessed_region_spatial_item m_accessed_region_spatial_item;
  std::unique_ptr<spatial_item> m_written_svalue_spatial_item;

  std::unique_ptr<boundaries> m_boundaries;

  bit_to_table_map m_btm;

  bool m_calc_req_size_called;

  /* Column widths shared by all x_aligned_table_widget,
     created once we know how many columns we need.  */
  std::unique_ptr<table_dimension_sizes> m_col_widths;

  /* All of the child x_aligned_table_widget that share
     column widths.  */
  std::vector<x_aligned_table_widget *> m_aligned_table_widgets;

/* Mapping from table_x to canvas_x.  */
  std::vector<int> m_col_start_x;
};

x_ruler
x_aligned_x_ruler_widget::make_x_ruler () const
{
  x_ruler r (x_ruler::label_dir::BELOW);
  for (auto& iter : m_labels)
    {
      canvas::range_t canvas_x_range
	= m_dia_impl.get_canvas_x_range (iter.m_table_x_range);
      /* Include the end-point.  */
      canvas_x_range.next++;
      r.add_label (canvas_x_range, iter.m_text.copy (), iter.m_style_id,
		   x_ruler::label_kind::TEXT_WITH_BORDER);
    }
  return r;
}

/* class direction_widget : public leaf_widget.  */

/* Paint arrows indicating the direction of the access (read vs write),
   but only in the X-extent corresponding to the region that's actually
   accessed.  */

void
direction_widget::paint_to_canvas (canvas &canvas)
{
  const access_range accessed_bits (m_dia_impl.get_op ().get_actual_bits ());

  const access_range valid_bits (m_dia_impl.get_op ().get_valid_bits ());

  for (unsigned table_x = 0; table_x < m_btm.get_num_columns (); table_x++)
    {
      access_range column_access_range;
      if (m_btm.maybe_get_access_range_for_table_x (table_x,
						    &column_access_range))
	{
	  /* Only paint arrows in the accessed region.  */
	  if (!accessed_bits.contains_p (column_access_range))
	    continue;

	  /* Are we within the valid region?  */
	  const bool is_valid (valid_bits.contains_p (column_access_range));
	  const style::id_t style_id
	    = m_dia_impl.get_style_id_for_validity (is_valid);
	  const canvas::range_t x_canvas_range
	    = m_dia_impl.get_canvas_x_range (table::range_t (table_x,
							     table_x + 1));
	  const int canvas_x = x_canvas_range.get_midpoint ();
	  m_dia_impl.get_theme ().paint_y_arrow
	    (canvas,
	     canvas_x,
	     canvas::range_t (get_y_range ()),
	     (m_dia_impl.get_op ().m_dir == DIR_READ
	      ? theme::y_arrow_dir::UP
	      : theme::y_arrow_dir::DOWN),
	     style_id);
	}
    }
}

/* class access_diagram : public text_art::wrapper_widget.  */

/* To hide the implementation details, this is merely a wrapper around
   an access_diagram_impl.  */

access_diagram::access_diagram (const access_operation &op,
				diagnostic_event_id_t region_creation_event_id,
				style_manager &sm,
				const theme &theme,
				logger *logger)
: wrapper_widget (make_unique <access_diagram_impl> (op,
						     region_creation_event_id,
						     sm,
						     theme,
						     logger))
{
}

#if CHECKING_P

namespace selftest {

/* Implementation detail of ASSERT_EQ_TYPELESS_INTEGER.  */

static void
assert_eq_typeless_integer (const location &loc,
			    const svalue *sval,
			    int expected_int_val)
{
  ASSERT_NE_AT (loc, sval, nullptr);
  ASSERT_EQ_AT (loc, sval->get_kind (), SK_CONSTANT);
  ASSERT_EQ_AT (loc,
		wi::to_offset (sval->maybe_get_constant ()),
		expected_int_val);
  ASSERT_EQ_AT (loc, sval->get_type (), NULL_TREE);
}

/* Assert that SVAL is a constant_svalue equal to EXPECTED_INT_VAL,
   with NULL_TREE as its type.  */

#define ASSERT_EQ_TYPELESS_INTEGER(SVAL, EXPECTED_INT_VAL) \
  SELFTEST_BEGIN_STMT						       \
  assert_eq_typeless_integer ((SELFTEST_LOCATION),		       \
			      (SVAL),				       \
			      (EXPECTED_INT_VAL));		       \
  SELFTEST_END_STMT


/* Various tests of bit_size_expr::maybe_get_as_bytes.  */

static void
test_bit_size_expr_to_bytes ()
{
  region_model_manager mgr;

  /* 40 bits: should be 5 bytes.  */
  {
    bit_size_expr num_bits (*mgr.get_or_create_int_cst (NULL_TREE, 40));
    const svalue *as_bytes = num_bits.maybe_get_as_bytes (mgr);
    ASSERT_EQ_TYPELESS_INTEGER (as_bytes, 5);
  }

  /* 41 bits: should not convert to bytes.  */
  {
    bit_size_expr num_bits (*mgr.get_or_create_int_cst (NULL_TREE, 41));
    const svalue *as_bytes = num_bits.maybe_get_as_bytes (mgr);
    ASSERT_EQ (as_bytes, nullptr);
  }

  tree n = build_global_decl ("n", size_type_node);

  const svalue *init_n
    = mgr.get_or_create_initial_value (mgr.get_region_for_global (n));

  const svalue *n_times_8
    = mgr.get_or_create_binop (NULL_TREE, MULT_EXPR,
			       init_n,
			       mgr.get_or_create_int_cst (NULL_TREE, 8));

  /* (n * 8) bits should be n bytes */
  {
    bit_size_expr num_bits (*n_times_8);
    const svalue *as_bytes = num_bits.maybe_get_as_bytes (mgr);
    ASSERT_EQ (as_bytes, mgr.get_or_create_cast (NULL_TREE, init_n));
  }

  /* (n * 8) + 16 bits should be n + 2 bytes */
  {
    bit_size_expr num_bits
      (*mgr.get_or_create_binop (NULL_TREE, PLUS_EXPR,
				 n_times_8,
				 mgr.get_or_create_int_cst (NULL_TREE, 16)));
    const svalue *as_bytes = num_bits.maybe_get_as_bytes (mgr);
    ASSERT_EQ (as_bytes->get_kind (), SK_BINOP);
    const binop_svalue *binop = as_bytes->dyn_cast_binop_svalue ();
    ASSERT_EQ (binop->get_op (), PLUS_EXPR);
    ASSERT_EQ (binop->get_arg0 (), mgr.get_or_create_cast (NULL_TREE, init_n));
    ASSERT_EQ_TYPELESS_INTEGER (binop->get_arg1 (), 2);
  }
}

/* Run all of the selftests within this file.  */

void
analyzer_access_diagram_cc_tests ()
{
  test_bit_size_expr_to_bytes ();
}

} // namespace selftest

#endif /* CHECKING_P */

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
