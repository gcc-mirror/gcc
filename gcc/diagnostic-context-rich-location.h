/* A rich_location subclass that lazily populates a diagnostic_path
   with diagnostic context events, but only if the path is actually to be
   used.
   Copyright (C) 2025-2026 Free Software Foundation, Inc.
   Contributed by Qing Zhao<qing.zhao@oracle.com>

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

#ifndef GCC_DIAGNOSTIC_CONTEXT_RICH_LOCATION_H
#define GCC_DIAGNOSTIC_CONTEXT_RICH_LOCATION_H

#include "gcc-rich-location.h"
#include "diagnostics/lazy-paths.h"
#include "tree-logical-location.h"

class lazy_diagnostic_context_path : public diagnostics::paths::lazy_path
{
public:
  lazy_diagnostic_context_path (const tree_logical_location_manager
				&logical_loc_mgr,
				location_t location, gimple *stmt)
  : diagnostics::paths::lazy_path (logical_loc_mgr),
    m_logical_loc_mgr (logical_loc_mgr),
    m_location (location), m_stmt (stmt)
  {
  }

  std::unique_ptr<diagnostics::paths::path>
  make_inner_path () const final override;
  /* This method will be called on demand if a diagnostic is actually
     emitted for this rich_location.  */

  const tree_logical_location_manager &m_logical_loc_mgr;
  location_t m_location;
  gimple *m_stmt;
};

class rich_location_with_details : public gcc_rich_location
{
public:
  rich_location_with_details (location_t location, gimple *stmt)
  : gcc_rich_location (location),
    m_lazy_diagnostic_context_path (m_logical_loc_mgr, location, stmt)
  {
    set_path (&m_lazy_diagnostic_context_path);
  }

  rich_location_with_details (location_t location, tree exp ATTRIBUTE_UNUSED)
  : gcc_rich_location (location),
    m_lazy_diagnostic_context_path (m_logical_loc_mgr, location, nullptr)
  {
  }

private:
  const tree_logical_location_manager m_logical_loc_mgr;
  lazy_diagnostic_context_path m_lazy_diagnostic_context_path;
};

#endif // GCC_DIAGNOSTIC_CONTEXT_RICH_LOCATION_H
