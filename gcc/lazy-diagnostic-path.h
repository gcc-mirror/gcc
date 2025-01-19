/* Helper class for deferring path creation until a diagnostic is emitted.
   Copyright (C) 2024-2025 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>

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

#ifndef GCC_LAZY_DIAGNOSTIC_PATH_H
#define GCC_LAZY_DIAGNOSTIC_PATH_H

#include "diagnostic-path.h"

/* An implementation of diagnostic_path which has a trivial ctor
   and lazily creates another diagnostic_path the first time the path
   is queried, deferring to this inner path for all queries.

   Use this to avoid expensive path creation logic when creating
   rich_location instances, so that expense can be deferred until the path
   is actually used by a diagnostic, and thus avoided for warnings that
   are disabled.  */

class lazy_diagnostic_path : public diagnostic_path
{
 public:
  virtual ~lazy_diagnostic_path () {}

  unsigned num_events () const final override;
  const diagnostic_event & get_event (int idx) const final override;
  unsigned num_threads () const final override;
  const diagnostic_thread &
  get_thread (diagnostic_thread_id_t) const final override;
  bool
  same_function_p (int event_idx_a,
		   int event_idx_b) const final override;

  bool generated_p () const { return m_inner_path != nullptr; }

 private:
  void lazily_generate_path () const;
  virtual std::unique_ptr<diagnostic_path> make_inner_path () const = 0;

  mutable std::unique_ptr<diagnostic_path> m_inner_path;
};

#endif /* ! GCC_LAZY_DIAGNOSTIC_PATH_H */
