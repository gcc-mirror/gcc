/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1" } */

unsigned foo (unsigned i)
{
  if (i == 2)
    {
      i = i << 2;
      if (i != 8)
	link_error ();
    }
  return i;
}
unsigned bar (unsigned i)
{
  if (i == 1 << (sizeof (unsigned) * 8 - 1))
    {
      i = i << 1;
      if (i != 0)
	link_error ();
    }
  return i;
}
unsigned baz (unsigned i)
{
  i = i & 15;
  if (i == 0)
    return 0;
  i = 1000 - i;
  i >>= 1;
  i <<= 1;
  if (i == 0)
    link_error ();
  return i;
}

/* { dg-final { scan-tree-dump-times "Folding predicate" 3 "vrp1" } } */
/* { dg-final { cleanup-tree-dump "vrp1" } } */
