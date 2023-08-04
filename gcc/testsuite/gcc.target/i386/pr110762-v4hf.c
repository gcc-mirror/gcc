/* PR target/110762 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mavx512fp16 -mavx512vl -dp" } */

typedef _Float16 v4hf __attribute__((vector_size(8)));
typedef _Float16 v2hf __attribute__((vector_size(4)));

v4hf
foo (v4hf a, v4hf b)
{
  return a + b;
}

v4hf
foo2 (v4hf a, v4hf b)
{
  return a - b;
}

v4hf
foo3 (v4hf a, v4hf b)
{
  return a * b;
}

v4hf
foo1 (v4hf a, v4hf b)
{
  return a / b;
}

v2hf
foo4 (v2hf a, v2hf b)
{
  return a + b;
}

v2hf
foo5 (v2hf a, v2hf b)
{
  return a - b;
}

v2hf
foo6 (v2hf a, v2hf b)
{
  return a * b;
}

v2hf
foo7 (v2hf a, v2hf b)
{
  return a / b;
}

/* { dg-final { scan-assembler-times "\\*vec_concatv8hf_0" 7 } } */
/* { dg-final { scan-assembler-times "\\*vec_concatv8hf_movss" 8 } } */
