/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gcv -mabi=lp64d" } */

#include <riscv_vector.h>

extern vint32m1_t get_vector (void);

/* The call returns in v8 and leaves VTYPE unspecified.  Establish a
   legal VTYPE before copying the result to v16.  */
void
foo (void)
{
  register vint32m1_t x asm ("v16") = get_vector ();
  asm volatile ("" : : "vr" (x));
}

/* { dg-final { scan-assembler {call\s+get_vector[^\n]*\n\s+vseti?vli\s+[^\n]+\n\s+vmv1r\.v\s+v16,v8} } } */
