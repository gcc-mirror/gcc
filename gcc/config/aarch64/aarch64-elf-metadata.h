/* ELF metadata for AArch64 architecture.
   Copyright (C) 2024-2025 Free Software Foundation, Inc.
   Contributed by ARM Ltd.

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

#ifndef GCC_AARCH64_ELF_METADATA_H
#define GCC_AARCH64_ELF_METADATA_H

namespace aarch64 {

class section_note_gnu_property
{
 public:
  section_note_gnu_property ();

  /* Add BTI flag to GNU properties.  */
  void bti_enabled ();
  /* Add GCS flag to GNU properties.  */
  void gcs_enabled ();
  /* Add PAC flag to GNU properties.  */
  void pac_enabled ();

  /* Write the data to the assembly file.  */
  void write () const;

 private:
  unsigned m_feature_1_and;
};

} // namespace aarch64

#endif /* GCC_AARCH64_ELF_METADATA_H */
