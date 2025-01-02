/* Subroutines used for code generation for RISC-V 'V' Extension for
   GNU compiler.
   Copyright (C) 2022-2025 Free Software Foundation, Inc.
   Contributed by Juzhe Zhong (juzhe.zhong@rivai.ai), RiVAI Technologies Ltd.

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

#ifndef GCC_RISCV_V_H
#define GCC_RISCV_V_H

#include "rtx-vector-builder.h"

using namespace riscv_vector;

namespace riscv_vector {

extern machine_mode get_mask_mode (machine_mode);
extern opt_machine_mode get_vector_mode (scalar_mode, poly_uint64);

class rvv_builder : public rtx_vector_builder
{
public:
  rvv_builder () : rtx_vector_builder () {}
  rvv_builder (machine_mode mode, unsigned int npatterns,
	       unsigned int nelts_per_pattern)
    : rtx_vector_builder (mode, npatterns, nelts_per_pattern)
  {
    m_inner_mode = GET_MODE_INNER (mode);
    m_inner_bits_size = GET_MODE_BITSIZE (m_inner_mode);
    m_inner_bytes_size = GET_MODE_SIZE (m_inner_mode);
    m_mask_mode = get_mask_mode (mode);

    gcc_assert (
      int_mode_for_size (inner_bits_size (), 0).exists (&m_inner_int_mode));
    m_int_mode
      = get_vector_mode (m_inner_int_mode, GET_MODE_NUNITS (mode)).require ();
  }

  bool can_duplicate_repeating_sequence_p ();
  bool is_repeating_sequence ();
  rtx get_merged_repeating_sequence ();

  bool repeating_sequence_use_merge_profitable_p ();
  bool combine_sequence_use_slideup_profitable_p ();
  bool combine_sequence_use_merge_profitable_p ();
  rtx get_merge_scalar_mask (unsigned int, machine_mode) const;

  bool single_step_npatterns_p () const;
  bool npatterns_all_equal_p () const;
  bool interleaved_stepped_npatterns_p () const;
  bool npatterns_vid_diff_repeated_p () const;

  machine_mode new_mode () const { return m_new_mode; }
  scalar_mode inner_mode () const { return m_inner_mode; }
  scalar_int_mode inner_int_mode () const { return m_inner_int_mode; }
  machine_mode mask_mode () const { return m_mask_mode; }
  machine_mode int_mode () const { return m_int_mode; }
  unsigned int inner_bits_size () const { return m_inner_bits_size; }
  unsigned int inner_bytes_size () const { return m_inner_bytes_size; }

private:
  scalar_mode m_inner_mode;
  scalar_int_mode m_inner_int_mode;
  machine_mode m_new_mode;
  scalar_int_mode m_new_inner_mode;
  machine_mode m_mask_mode;
  machine_mode m_int_mode;
  unsigned int m_inner_bits_size;
  unsigned int m_inner_bytes_size;
};

extern bool valid_vec_immediate_p(rtx);

} // namespace riscv_vector

#endif // GCC_RISCV_V_H
