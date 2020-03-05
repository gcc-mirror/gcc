/* PR rtl-optimization/91720 */
/* { dg-do run } */
/* { dg-options "-Og -fno-forward-propagate -frerun-cse-after-loop -fno-tree-fre" } */

unsigned a, b;

int
main ()
{
#if __CHAR_BIT__ == 8
  unsigned c = 1;
  unsigned long long d = 0;
  unsigned char e = 0;
  e = __builtin_sub_overflow (d, e, &a) ? 0 : 0x80;
  e = e << 7 | e >> c;
  __builtin_memmove (&d, &a, 2);
  b = e;
  if (b != 0x40)
    __builtin_abort ();
#endif
  return 0;
}
