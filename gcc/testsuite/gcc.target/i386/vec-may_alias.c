/* { dg-do run } */
/* { dg-options "-O2 -w -Wno-abi" } */

typedef int v2si __attribute__ ((vector_size (8)));
typedef short v4hi __attribute__ ((vector_size (8)));
typedef short v4hia __attribute__ ((vector_size (8), may_alias));

__attribute__ ((noinline, noclone))
int f (v2si A, int N)
{ return ((v4hia)A)[N]; }

__attribute__ ((noinline, noclone))
int g (v2si A, int N)
{ return ((v4hi)A)[N]; }

int main()
{
  v2si x = { 0, 0 }, y = { 1, 1 };
  if (f (x, 0) || f (x, 1) || f (x, 2) || f (x, 3))
    __builtin_abort ();
  if (g (y, 0) != 1 || g (y, 1) || g (y, 2) != 1 || g (y, 3))
    __builtin_abort ();
  return 0;
}

