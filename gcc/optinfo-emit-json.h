/* Emit optimization information as JSON files.
   Copyright (C) 2018 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_OPTINFO_EMIT_JSON_H
#define GCC_OPTINFO_EMIT_JSON_H

class optinfo;
struct opt_pass;

extern void optimization_records_start ();
extern void optimization_records_finish ();

extern bool optimization_records_enabled_p ();

extern void optimization_records_maybe_record_optinfo (const optinfo *);
extern void optimization_records_maybe_pop_dump_scope ();


#endif /* #ifndef GCC_OPTINFO_EMIT_JSON_H */
