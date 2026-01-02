/* Various declarations for language-independent diagnostics subroutines.
   Copyright (C) 2000-2026 Free Software Foundation, Inc.

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

#ifndef GCC_DIAGNOSTICS_DIAGNOSTIC_INFO_H
#define GCC_DIAGNOSTICS_DIAGNOSTIC_INFO_H

namespace diagnostics {

class metadata;

/* A diagnostic is described by the MESSAGE to send, the FILE and LINE of
   its context and its KIND (ice, error, warning, note, ...)  See complete
   list in diagnostics/kinds.def.  */

struct diagnostic_info
{
  diagnostic_info ()
  : m_message (),
    m_richloc (),
    m_metadata (),
    m_x_data (),
    m_kind (),
    m_option_id (),
    m_iinfo ()
  { }

  /* Text to be formatted.  */
  text_info m_message;

  /* The location at which the diagnostic is to be reported.  */
  rich_location *m_richloc;

  /* An optional bundle of metadata associated with the diagnostic
     (or NULL).  */
  const metadata *m_metadata;

  /* Auxiliary data for client.  */
  void *m_x_data;
  /* The kind of diagnostic it is about.  */
  kind m_kind;
  /* Which OPT_* directly controls this diagnostic.  */
  option_id m_option_id;

  /* Inlining context containing locations for each call site along
     the inlining stack.  */
  struct inlining_info
  {
    /* Locations along the inlining stack.  */
    auto_vec<location_t, 8> m_ilocs;
    /* The abstract origin of the location.  */
    void *m_ao;
    /* Set if every M_ILOCS element is in a system header.  */
    bool m_allsyslocs;
  } m_iinfo;
};

} // namespace diagnostics

#endif /* ! GCC_DIAGNOSTICS_DIAGNOSTIC_INFO_H */
