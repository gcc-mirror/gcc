/* { dg-do compile } */
/* { dg-options "-mavx512fp16 -mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vpblendmw" 1 } }  */

typedef short v8hi __attribute__((vector_size(16)));
typedef __bf16 v8bf __attribute__((vector_size(16)));

v8bf
foo (v8hi a, v8hi b, v8bf c, v8bf d)
{
      return a > b ? c : d;
}

