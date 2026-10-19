/* PR rtl-optimization/127903 */
/* { dg-do run } */
/* { dg-require-effective-target int32 } */
/* { dg-additional-options "-Wno-psabi" } */

typedef float v4sf __attribute__((vector_size (16)));
typedef int v4si __attribute__((vector_size (16)));

__attribute__((noipa)) v4sf
unord (v4sf a, v4sf b)
{
  v4si m;
  for (int i = 0; i < 4; i++)
    m[i] = __builtin_isunordered (a[i], b[i]) ? -1 : 0;
  return (v4sf) m;
}

__attribute__((noipa)) v4sf
ord (v4sf a, v4sf b)
{
  v4si m;
  for (int i = 0; i < 4; i++)
    m[i] = __builtin_isunordered (a[i], b[i]) ? 0 : -1;
  return (v4sf) m;
}

int
main ()
{
  v4sf a = { __builtin_nanf (""), 1.0f, 2.0f, 3.0f };
  v4sf b = { 0.0f, 1.0f, 2.0f, __builtin_nanf ("") };
  v4si u = (v4si) unord (a, b);
  v4si o = (v4si) ord (a, b);
  if (u[0] != -1 || u[1] != 0 || u[2] != 0 || u[3] != -1)
    __builtin_abort ();
  if (o[0] != 0 || o[1] != -1 || o[2] != -1 || o[3] != 0)
    __builtin_abort ();
  return 0;
}
