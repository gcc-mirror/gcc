/* { dg-do compile } */
/* { dg-options "-march=x86-64" } */
/* { dg-final { scan-assembler "%zmm" } } */

typedef double __m512d __attribute__ ((__vector_size__ (64), __may_alias__));

__attribute__ ((target ("avx512f,no-avx10.1"))) __m512d
foo ()
{ /* { dg-warning "'-mno-avx10.1' is ignored when using with '-mavx512{f,vl,bw,dq,cd,bf16,fp16,vbmi,vbmi2,vnni,ifma,bitalg,vpopcntdq}'" } */
  __m512d a, b;
  a = a + b;
  return a;
}
