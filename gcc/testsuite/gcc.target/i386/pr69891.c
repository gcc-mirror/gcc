/* PR rtl-optimization/69891 */
/* { dg-do run } */
/* { dg-options "-O -fno-tree-fre -mstringop-strategy=libcall -Wno-psabi" } */
/* { dg-additional-options "-mno-sse" { target ia32 } } */

typedef unsigned short A;
typedef unsigned short B __attribute__ ((vector_size (32)));
typedef unsigned int C;
typedef unsigned int D __attribute__ ((vector_size (32)));
typedef unsigned long long E;
typedef unsigned long long F __attribute__ ((vector_size (32)));

__attribute__((noinline, noclone)) unsigned
foo(D a, B b, D c, F d)
{
  b /= (B) {1, -c[0]} | 1;
  c[0] |= 7;
  a %= c | 1;
  c ^= c;
  return a[0] + b[15] + c[0] + d[3];
}

int
main ()
{
  unsigned x = foo ((D) {}, (B) {}, (D) {}, (F) {});
  if (x != 0)
    __builtin_abort ();
  return 0;
}
