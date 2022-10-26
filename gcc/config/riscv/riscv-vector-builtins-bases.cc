/* function_base implementation for RISC-V 'V' Extension for GNU compiler.
   Copyright (C) 2022-2022 Free Software Foundation, Inc.
   Contributed by Ju-Zhe Zhong (juzhe.zhong@rivai.ai), RiVAI Technologies Ltd.

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
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "rtl.h"
#include "tm_p.h"
#include "memmodel.h"
#include "insn-codes.h"
#include "optabs.h"
#include "recog.h"
#include "expr.h"
#include "basic-block.h"
#include "function.h"
#include "fold-const.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "gimplify.h"
#include "explow.h"
#include "emit-rtl.h"
#include "tree-vector-builder.h"
#include "rtx-vector-builder.h"
#include "riscv-vector-builtins.h"
#include "riscv-vector-builtins-shapes.h"
#include "riscv-vector-builtins-bases.h"

using namespace riscv_vector;

namespace riscv_vector {

/* Implements vsetvl<mode> && vsetvlmax<mode>.  */
template<bool VLMAX_P>
class vsetvl : public function_base
{
public:
  unsigned int call_properties (const function_instance &) const
  {
    return CP_READ_CSR | CP_WRITE_CSR;
  }

  rtx expand (function_expander &e) const override
  {
    if (VLMAX_P)
      e.add_input_operand (Pmode, gen_rtx_REG (Pmode, 0));
    else
      e.add_input_operand (0);

    tree type = builtin_types[e.type.index].vector;
    machine_mode mode = TYPE_MODE (type);
    machine_mode inner_mode = GET_MODE_INNER (mode);
    /* SEW.  */
    e.add_input_operand (Pmode,
			 gen_int_mode (GET_MODE_BITSIZE (inner_mode), Pmode));

    /* LMUL.  */
    e.add_input_operand (Pmode, gen_int_mode ((unsigned int) mode, Pmode));

    /* TA.  */
    e.add_input_operand (Pmode, gen_int_mode (1, Pmode));

    /* MU.  */
    e.add_input_operand (Pmode, gen_int_mode (0, Pmode));
    return e.generate_insn (code_for_vsetvl (Pmode));
  }
};

static constexpr const vsetvl<false> vsetvl_obj;
static constexpr const vsetvl<true> vsetvlmax_obj;
namespace bases {
const function_base *const vsetvl = &vsetvl_obj;
const function_base *const vsetvlmax = &vsetvlmax_obj;
}

} // end namespace riscv_vector
