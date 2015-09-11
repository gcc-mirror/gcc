/* PR tree-optimization/64322 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1" } */

extern void link_error ();
extern void required_check ();

long long int
foo (long long int x)
{
  x >>= sizeof (long long int) * __CHAR_BIT__ - 1;
  if (x != 0 && x != -1)
    link_error ();
  return x;
}

unsigned long long int
bar (unsigned long long int x)
{
  x >>= sizeof (long long int) * __CHAR_BIT__ - 1;
  if (x != 0 && x != 1)
    link_error ();
  return x;
}

long long int
baz (long long int x)
{
  x = (x >> sizeof (long long int) * __CHAR_BIT__ - 1) << 1;
  x = x / 0x100000000LL;
  if (x != 0)
    link_error ();
  return x;
}

unsigned long long int
range (unsigned long long int x, int y)
{
  y &= 3;
  x >>= sizeof (long long int) * __CHAR_BIT__ - 1 - y;
  if (x > 15)
    link_error ();
  if (x == 15)
    required_check ();
  return x;
}

/* { dg-final { scan-tree-dump-not "link_error" "vrp1" } } */
/* { dg-final { scan-tree-dump "required_check" "vrp1" } } */
