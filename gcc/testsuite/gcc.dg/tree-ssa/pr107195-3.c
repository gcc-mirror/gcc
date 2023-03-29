/* Inspired by 'libgomp.oacc-c-c++-common/nvptx-sese-1.c'.  */

/* { dg-additional-options -O1 } */
/* { dg-additional-options -fdump-tree-dom3-raw } */


extern int
__attribute__((const))
foo1 (int);

int f1 (int r)
{
  if (foo1 (r)) /* If this first 'if' holds...  */
    r *= 2; /* ..., 'r' now has a zero-value lower-most bit...  */

  if (r & 1) /* ..., so this second 'if' can never hold...  */
    { /* ..., so this is unreachable.  */
      /* In constrast, if the first 'if' does not hold ('foo1 (r) == 0'), the
	 second 'if' may hold, but we know ('foo1' being 'const') that
	 'foo1 (r) == 0', so don't have to re-evaluate it here: */
      r += foo1 (r);
    }

  return r;
}
/* Thus, if optimizing, we only ever expect one call of 'foo1'.
   { dg-final { scan-tree-dump-times {gimple_call <foo1,} 1 dom3 } } */


extern int
__attribute__((const))
foo2 (int);

int f2 (int r)
{
  if (foo2 (r))
    r *= 8;

  if (r & 7)
    r += foo2 (r);

  return r;
}
/* { dg-final { scan-tree-dump-times {gimple_call <foo2,} 1 dom3 } } */


extern int
__attribute__((const))
foo3 (int);

int f3 (int r)
{
  if (foo3 (r))
    r <<= 4;

  if ((r & 64) && ((r & 8) || (r & 4) || (r & 2) || (r & 1)))
    r += foo3 (r);

  return r;
}
/* { dg-final { scan-tree-dump-times {gimple_call <foo3,} 1 dom3 } } */


extern int
__attribute__((const))
foo4 (int);

int f4 (int r)
{
  if (foo4 (r))
    r *= 8;

  if ((r >> 1) & 2)
    r += foo4 (r);

  return r;
}
/* { dg-final { scan-tree-dump-times {gimple_call <foo4,} 1 dom3 } } */


extern int
__attribute__((const))
foo5 (int);

int f5 (int r) /* Works for both 'signed' and 'unsigned'.  */
{
  if (foo5 (r))
    r *= 2;

  if ((r % 2) != 0)
    r += foo5 (r);

  return r;
}
/* { dg-final { scan-tree-dump-times {gimple_call <foo5,} 1 dom3 } } */


extern int
__attribute__((const))
foo6 (int);

int f6 (unsigned int r) /* 'unsigned' is important here.  */
{
  if (foo6 (r))
    r *= 2;

  if ((r % 2) == 1)
    r += foo6 (r);

  return r;
}
/* { dg-final { scan-tree-dump-times {gimple_call <foo6,} 1 dom3 } } */
