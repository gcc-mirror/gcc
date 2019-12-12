/* { dg-do compile } */
/* { dg-options "-O3 -mzarch -march=arch13 -mzvector -fno-asynchronous-unwind-tables -dp" } */

#include <vecintrin.h>

vector signed int
vcfeb (vector float a)
{
  return vec_signed (a);
}

/* { dg-final { scan-assembler-times "vcfeb.*\n\tvcfeb.*fix_truncv4sfv4si2" 1 } } */

vector signed long long
vcgdb (vector double a)
{
  return vec_signed (a);
}

/* { dg-final { scan-assembler-times "vcgdb.*\n\tvcgdb.*fix_truncv2dfv2di2" 1 } } */

vector signed int
vcfeb_mem (vector float *a)
{
  return vec_signed (*a);
}

vector signed long long
vcgdb_mem (vector double *a)
{
  return vec_signed (*a);
}

vector signed int
vcfeb_imm ()
{
  return vec_signed ((vector float) { 1.0f, 2.0f });
}

vector signed long long
vcgdb_imm ()
{
  return vec_signed ((vector double){ 1.0, 2.0 });
}

/* { dg-final { scan-assembler-times "vcfeb\t" 3 } } */
/* { dg-final { scan-assembler-times "vcgdb\t" 3 } } */
