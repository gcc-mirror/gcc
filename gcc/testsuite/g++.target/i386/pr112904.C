// PR target/112904
// { dg-do compile }
// { dg-options "-mxop" }
// { dg-additional-options "-mmmx" { target ia32 } }

typedef _Float16 v4hf __attribute__((vector_size(8)));
typedef short v4hi __attribute__((vector_size(8)));
typedef _Float16 v2hf __attribute__((vector_size(4)));
typedef short v2hi __attribute__((vector_size(4)));

typedef __bf16 v4bf __attribute__((vector_size(8)));
typedef __bf16 v2bf __attribute__((vector_size(4)));

v4hf foo(v4hf a, v4hf b, v4hi c)
{
  return c ? a : b;
}

v2hf foo1(v2hf a, v2hf b, v2hi c)
{
  return c ? a : b;
}

v4bf foo(v4bf a, v4bf b, v4hi c)
{
  return c ? a : b;
}

v2bf foo1(v2bf a, v2bf b, v2hi c)
{
  return c ? a : b;
}
