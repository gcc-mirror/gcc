/* Check that vector insertion works correctly. */

/* { dg-do run } */
/* { dg-options "-msse" { target { i?86-*-* x86_64-*-* } } } */
/* { dg-require-effective-target sse_runtime { target { i?86-*-* x86_64-*-* } } } */
/* { dg-options "-mabi=altivec" { target { powerpc-*-* powerpc64-*-* } } } */
/* { dg-require-effective-target vmx_hw { target { powerpc-*-* powerpc64--*-* } } } */

#define vector __attribute__((vector_size(16) ))

vector int f0(vector int t, int a)
{
 ((int*)&t)[0] = a;
 return t;
}
vector int f1(vector int t, int a)
{
 ((int*)&t)[1] = a;
 return t;
}
vector int f2(vector int t, int a)
{
 ((int*)&t)[2] = a;
 return t;
}
vector int f3(vector int t, int a)
{
 ((int*)&t)[3] = a;
 return t;
}
int main(void)
{
  vector int a = {0, 0, 0, 0};
  vector int b = {1, 0, 0, 0};
  vector int c = {0, 1, 0, 0};
  vector int d = {0, 0, 1, 0};
  vector int e = {0, 0, 0, 1};
  vector int a0;
  a0 = f0(a, 1);
  if (memcmp (&a0, &b, sizeof(a0)))
    __builtin_abort ();
  a0 = f1(a, 1);
  if (memcmp (&a0, &c, sizeof(a0)))
    __builtin_abort ();
  a0 = f2(a, 1);
  if (memcmp (&a0, &d, sizeof(a0)))
    __builtin_abort ();
  a0 = f3(a, 1);
  if (memcmp (&a0, &e, sizeof(a0)))
    __builtin_abort ();
  return 0;
}
