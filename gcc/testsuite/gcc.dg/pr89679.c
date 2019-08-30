/* PR rtl-optimization/89679 */
/* { dg-do run } */
/* { dg-options "-Og -frerun-cse-after-loop -fno-tree-fre" } */

unsigned short g;

void
foo (unsigned long long x)
{
  if (x != 0xffff)
    __builtin_abort ();
}

int
main ()
{
#if __SIZEOF_SHORT__ == 2 && __SIZEOF_INT__ == 4 && __CHAR_BIT__ == 8
  unsigned short d = 0;
  unsigned long long x, c = ~0;
  c = c >> d;
  __builtin_memset (&d, c, 2);
  x = d + g;
  foo (x);
#endif
  return 0;
}
