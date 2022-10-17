/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */


void bar (void);

void
foo(unsigned int abc123)
{
  unsigned int xyzpdq = (1 << abc123);
  if ((xyzpdq & 0x800) != 0)
    bar();
}

void
baz(unsigned int abc123)
{
  unsigned int xyzpdq = (1 << abc123);
  if ((xyzpdq & 0x800) == 0)
    bar();
}

/* What we want to verify is that the bit test against xyzpdq is
   replaced with a test against abc123 which avoids the shifting
   and bit ops.  */
/* { dg-final { scan-tree-dump-not "xyzpdq" "optimized"} } */
/* { dg-final { scan-tree-dump-times "if .abc123" 2 "optimized"} } */
