// { dg-options "-O1 -fdump-tree-dom3-raw" }

extern int
__attribute__((const))
foo4b (int);

int f4b (unsigned int r)
{
  if (foo4b (r))
    r *= 8U;

  if ((r / 2U) & 2U)
    r += foo4b (r);

  return r;
}

// { dg-final { scan-tree-dump-times {gimple_call <foo4b,} 1 dom3 } }
