/* Support code for handling the various dump_* calls in dumpfile.h
   Copyright (C) 2018 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */


#ifndef GCC_DUMP_CONTEXT_H
#define GCC_DUMP_CONTEXT_H 1

#include "dumpfile.h"
#include "pretty-print.h"

/* A class for handling the various dump_* calls.

   In particular, this class has responsibility for consolidating
   the "dump_*" calls into optinfo instances (delimited by "dump_*_loc"
   calls), and emitting them.

   Putting this in a class (rather than as global state) allows
   for selftesting of this code.  */

class dump_context
{
  friend class temp_dump_context;
 public:
  static dump_context &get () { return *s_current; }

  ~dump_context ();

  void refresh_dumps_are_enabled ();

  void dump_loc (dump_flags_t dump_kind, const dump_location_t &loc);

  void dump_gimple_stmt (dump_flags_t dump_kind, dump_flags_t extra_dump_flags,
			 gimple *gs, int spc);

  void dump_gimple_stmt_loc (dump_flags_t dump_kind,
			     const dump_location_t &loc,
			     dump_flags_t extra_dump_flags,
			     gimple *gs, int spc);

  void dump_gimple_expr (dump_flags_t dump_kind,
			 dump_flags_t extra_dump_flags,
			 gimple *gs, int spc);

  void dump_gimple_expr_loc (dump_flags_t dump_kind,
			    const dump_location_t &loc,
			    dump_flags_t extra_dump_flags,
			    gimple *gs,
			    int spc);

  void dump_generic_expr (dump_flags_t dump_kind,
			  dump_flags_t extra_dump_flags,
			  tree t);

  void dump_generic_expr_loc (dump_flags_t dump_kind,
			      const dump_location_t &loc,
			      dump_flags_t extra_dump_flags,
			      tree t);

  void dump_printf_va (dump_flags_t dump_kind, const char *format,
		       va_list *ap) ATTRIBUTE_GCC_DUMP_PRINTF (3, 0);

  void dump_printf_loc_va (dump_flags_t dump_kind, const dump_location_t &loc,
			   const char *format, va_list *ap)
    ATTRIBUTE_GCC_DUMP_PRINTF (4, 0);

  template<unsigned int N, typename C>
  void dump_dec (dump_flags_t dump_kind, const poly_int<N, C> &value);

  void dump_symtab_node (dump_flags_t dump_kind, symtab_node *node);

  /* Managing nested scopes.  */
  unsigned int get_scope_depth () const;
  void begin_scope (const char *name, const dump_location_t &loc);
  void end_scope ();

  /* For use in selftests; if true then optinfo_enabled_p is true.  */
  bool forcibly_enable_optinfo_p () const
  {
    return m_forcibly_enable_optinfo;
  }

  void end_any_optinfo ();

  void emit_item (optinfo_item *item, dump_flags_t dump_kind);

 private:
  optinfo &ensure_pending_optinfo ();
  optinfo &begin_next_optinfo (const dump_location_t &loc);

  /* For use in selftests; if true then optinfo_enabled_p is true.  */
  bool m_forcibly_enable_optinfo;

  /* The current nesting depth of dump scopes, for showing nesting
     via indentation).  */
  unsigned int m_scope_depth;

  /* The optinfo currently being accumulated since the last dump_*_loc call,
     if any.  */
  optinfo *m_pending;

  /* For use in selftests: if non-NULL, then items are to be printed
     to this, using the given flags.  */
  pretty_printer *m_test_pp;
  dump_flags_t m_test_pp_flags;

  /* The currently active dump_context, for use by the dump_* API calls.  */
  static dump_context *s_current;

  /* The default active context.  */
  static dump_context s_default;
};

#if CHECKING_P

/* An RAII-style class for use in selftests for temporarily using a different
   dump_context.  */

class temp_dump_context
{
 public:
  temp_dump_context (bool forcibly_enable_optinfo,
		     dump_flags_t test_pp_flags);
  ~temp_dump_context ();

  /* Support for selftests.  */
  optinfo *get_pending_optinfo () const { return m_context.m_pending; }
  const char *get_dumped_text ();

 private:
  pretty_printer m_pp;
  dump_context m_context;
  dump_context *m_saved;
  bool m_saved_flag_remarks;
};

#endif /* CHECKING_P */

#endif /* GCC_DUMP_CONTEXT_H */
