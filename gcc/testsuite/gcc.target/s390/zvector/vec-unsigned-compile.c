/* { dg-do compile } */
/* { dg-options "-O3 -mzarch -march=arch13 -mzvector -fno-asynchronous-unwind-tables -dp" } */

#include <vecintrin.h>

vector unsigned int
vclfeb (vector float a)
{
  return vec_unsigned (a);
}

/* { dg-final { scan-assembler-times "vclfeb.*\n\tvclfeb.*fixuns_truncv4sfv4si2" 1 } } */

vector unsigned long long
vclgdb (vector double a)
{
  return vec_unsigned (a);
}

/* { dg-final { scan-assembler-times "vclgdb.*\n\tvclgdb.*fixuns_truncv2dfv2di2" 1 } } */

vector unsigned int
vclfeb_mem (vector float *a)
{
  return vec_unsigned (*a);
}

vector unsigned long long
vclgdb_mem (vector double *a)
{
  return vec_unsigned (*a);
}

/* The following immediates are being converted and directly stored
   in the literal pool so no explicit conversion is necessary.   */
/* { dg-final { scan-assembler-times "vl\t%v\[0-9\]+,\.L\[0-9\]+\-\.L\[0-9\]+\\(%r\[0-9\]+\\)" 2 } } */

vector unsigned int
vclfeb_imm ()
{
  return vec_unsigned ((vector float) { 1.0f, 2.0f });
}

vector unsigned long long
vclgdb_imm ()
{
  return vec_unsigned ((vector double){ 1.0, 2.0 });
}

/* { dg-final { scan-assembler-times "vclfeb\t" 2 } } */
/* { dg-final { scan-assembler-times "vclgdb\t" 2 } } */
