/* Concrete subclass of logical_location for use in selftests.
   Copyright (C) 2024-2025 Free Software Foundation, Inc.
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

#ifndef GCC_SELFTEST_LOGICAL_LOCATION_H
#define GCC_SELFTEST_LOGICAL_LOCATION_H

#include "logical-location.h"

/* The selftest code should entirely disappear in a production
   configuration, hence we guard all of it with #if CHECKING_P.  */

#if CHECKING_P

namespace selftest {

/* Concrete subclass of logical_location for use in selftests.  */

class test_logical_location : public logical_location
{
public:
  test_logical_location (enum logical_location_kind kind,
			 const char *name);
  virtual const char *get_short_name () const final override;
  virtual const char *get_name_with_scope () const final override;
  virtual const char *get_internal_name () const final override;
  virtual enum logical_location_kind get_kind () const final override;
  virtual label_text get_name_for_path_output () const final override;

  const char *get_name () const { return m_name; }

 private:
  enum logical_location_kind m_kind;
  const char *m_name;
};

} // namespace selftest

#endif /* #if CHECKING_P */


#endif /* GCC_SELFTEST_LOGICAL_LOCATION_H.  */
