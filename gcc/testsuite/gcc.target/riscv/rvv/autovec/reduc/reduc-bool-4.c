/* { dg-do compile } */
/* { dg-additional-options "-march=rv64gcv -mabi=lp64d -fdump-tree-vect-details" } */

long long p[128];

bool __attribute__((noipa))
fand ()
{
  bool r = true;
  for (int i = 0; i < 16; ++i)
    r &= (p[i] != 0);
  return r;
}

bool __attribute__((noipa))
fior ()
{
  bool r = false;
  for (int i = 0; i < 16; ++i)
    r |= (p[i] != 0);
  return r;
}

/* { dg-final { scan-tree-dump-times "optimized: loop vectorized" 2 "vect" } } */
/* { dg-final { scan-assembler-times {vcpop\.m\s+[atx][0-9]+,\s*v[0-9]+} 2 } } */
