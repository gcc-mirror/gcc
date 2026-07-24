/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_0(
  __riscv_vsetvlmax_e16m1,
  vint16mf2_t,
  vint32m1_t,
  __riscv_vle16_v_i16mf2,
  __riscv_vle32_v_i32m1,
  __riscv_vwadd_wv_i32m1,
  __riscv_vse32_v_i32m1,
  vwadd_wv,
  LOOP_WIDEN_BINARY_BODY_X16)

/* { dg-final { scan-assembler-not {vwadd\.wv\s+v0,v[0-9]+,v1([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwadd\.wv\s+v2,v[0-9]+,v3([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwadd\.wv\s+v4,v[0-9]+,v5([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwadd\.wv\s+v6,v[0-9]+,v7([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwadd\.wv\s+v8,v[0-9]+,v9([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwadd\.wv\s+v10,v[0-9]+,v11([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwadd\.wv\s+v12,v[0-9]+,v13([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwadd\.wv\s+v14,v[0-9]+,v15([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwadd\.wv\s+v16,v[0-9]+,v17([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwadd\.wv\s+v18,v[0-9]+,v19([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwadd\.wv\s+v20,v[0-9]+,v21([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwadd\.wv\s+v22,v[0-9]+,v23([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwadd\.wv\s+v24,v[0-9]+,v25([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwadd\.wv\s+v26,v[0-9]+,v27([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwadd\.wv\s+v28,v[0-9]+,v29([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwadd\.wv\s+v30,v[0-9]+,v31([^0-9]|$)} } } */
