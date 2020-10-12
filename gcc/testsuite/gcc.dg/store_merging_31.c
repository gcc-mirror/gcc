/* PR tree-optimization/97053 */
/* { dg-do run } */
/* { dg-options "-O2" } */

struct S { short a; char b[9]; int c; char d; int e; };

__attribute__((noipa)) void
foo (char *x, char *y)
{
  if (__builtin_strcmp (x, "ABCDXXXX") != 0
      || __builtin_strcmp (y, "ABCDXXXX") != 0)
    __builtin_abort ();
}

int
main ()
{
  char a[9] = "XXXXXXXX";
  struct S b = {};
  __builtin_memcpy (a, "ABCD", 4);
  b.a = 5;
  __builtin_memcpy (b.b, a, 8); 
  b.d = 'X';
  b.e = 1;
  foo (a, b.b);
  return 0;
}
