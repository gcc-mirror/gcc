/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64 -O2" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" "-Os" "-Oz" } } */

#include "riscv_vector.h"

extern void can_clobber_vtype();

static inline void v_loop (void * restrict in, void * restrict out, int n)
{
  for (int i = 0; i < n; i++)
    {
      vuint8mf8_t v = *(vuint8mf8_t*)(in + i);
      *(vuint8mf8_t*)(out + i) = v;
    }
}

/* Two V instructions back-back.
   Only 1 vsetvli insn.  */
void
vec1 (void * restrict in, void * restrict out1,  void * restrict out2, int n)
{
     v_loop(in, out1, n);
     v_loop(in, out2, n);
}

/* Two V instructions seperated by a function call.
   Both need to have a corresponding vsetvli insn.  */
void
vec2 (void * restrict in, void * restrict out1,  void * restrict out2, int n)
{
     v_loop(in, out1, n);
     can_clobber_vtype();
     v_loop(in, out2, n);
}

/* Two V instructions seperated by an inline asm with vtype clobber.
   Both need to have a corresponding vsetvli insn.  */
void
vec3 (void * restrict in, void * restrict out1,  void * restrict out2, int n)
{
     v_loop(in, out1, n);
     asm volatile("":::"vtype");
     v_loop(in, out2, n);
}

/* { dg-final { scan-assembler-times {vsetvli} 5 } } */
