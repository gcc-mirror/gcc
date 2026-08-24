/* { dg-do compile } */
/* { dg-options "-O3 -msve-vector-bits=scalable -mautovec-preference=sve-only -fvect-cost-model=unlimited --param=vect-partial-vector-usage=0 -fdump-tree-vect-details -fdump-tree-forwprop4" } */

/* Check reductions of scalable uniform vector constants.  */

char c1, c2, c3, c4, c5;
short d1, d2, d3, d4, d5;
int rmax, rmin, r_and, r_ior, r_xor;
unsigned n;

__attribute__ ((noipa)) void
max_uniform (void)
{
  for (short i = 0; i < n; i += 3)
    {
      c1 ^= d1;
      rmax = rmax > 17 ? rmax : 17;
    }
}

__attribute__ ((noipa)) void
min_uniform (void)
{
  for (short i = 0; i < n; i += 3)
    {
      c2 ^= d2;
      rmin = rmin < -19 ? rmin : -19;
    }
}

__attribute__ ((noipa)) void
and_uniform (void)
{
  for (short i = 0; i < n; i += 3)
    {
      c3 ^= d3;
      r_and &= 1431655765;
    }
}

__attribute__ ((noipa)) void
ior_uniform (void)
{
  for (short i = 0; i < n; i += 3)
    {
      c4 ^= d4;
      r_ior |= 572662306;
    }
}

__attribute__ ((noipa)) void
xor_uniform (void)
{
  for (short i = 0; i < n; i += 3)
    {
      c5 ^= d5;
      r_xor ^= 858993459;
    }
}

/* { dg-final { scan-tree-dump-times {LOOP VECTORIZED} 5 "vect" } } */
/* { dg-final { scan-tree-dump-times {\.REDUC_MAX} 1 "forwprop4" } } */
/* { dg-final { scan-tree-dump-times {\.REDUC_MIN} 1 "forwprop4" } } */
/* { dg-final { scan-tree-dump-times {\.REDUC_AND} 1 "forwprop4" } } */
/* { dg-final { scan-tree-dump-times {\.REDUC_IOR} 1 "forwprop4" } } */
/* { dg-final { scan-tree-dump-times {\.REDUC_MAX \(vect_rmax_lsm} 1 "forwprop4" } } */
/* { dg-final { scan-tree-dump-times {\.REDUC_MIN \(vect_rmin_lsm} 1 "forwprop4" } } */
/* { dg-final { scan-tree-dump-times {\.REDUC_AND \(vect_r_and_lsm} 1 "forwprop4" } } */
/* { dg-final { scan-tree-dump-times {\.REDUC_IOR \(vect_r_ior_lsm} 1 "forwprop4" } } */
/* { dg-final { scan-tree-dump-times {\.REDUC_XOR} 5 "forwprop4" } } */
/* { dg-final { scan-tree-dump-not {\.REDUC_XOR \(\{ 0, \.\.\. \}\)} "forwprop4" } } */
