/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_0(
  __riscv_vsetvlmax_e32m1,
  vint32mf2_t,
  vint64m1_t,
  __riscv_vle32_v_i32mf2,
  __riscv_vle64_v_i64m1,
  __riscv_vwsub_wv_i64m1,
  __riscv_vse64_v_i64m1,
  vwsub_wv,
  LOOP_WIDEN_BINARY_BODY_X16)

/* { dg-final { scan-assembler-not {vwsub\.wv\s+v0,v[0-9]+,v1([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwsub\.wv\s+v2,v[0-9]+,v3([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwsub\.wv\s+v4,v[0-9]+,v5([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwsub\.wv\s+v6,v[0-9]+,v7([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwsub\.wv\s+v8,v[0-9]+,v9([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwsub\.wv\s+v10,v[0-9]+,v11([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwsub\.wv\s+v12,v[0-9]+,v13([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwsub\.wv\s+v14,v[0-9]+,v15([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwsub\.wv\s+v16,v[0-9]+,v17([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwsub\.wv\s+v18,v[0-9]+,v19([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwsub\.wv\s+v20,v[0-9]+,v21([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwsub\.wv\s+v22,v[0-9]+,v23([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwsub\.wv\s+v24,v[0-9]+,v25([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwsub\.wv\s+v26,v[0-9]+,v27([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwsub\.wv\s+v28,v[0-9]+,v29([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwsub\.wv\s+v30,v[0-9]+,v31([^0-9]|$)} } } */
