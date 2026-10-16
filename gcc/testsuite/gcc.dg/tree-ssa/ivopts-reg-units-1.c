/* ivopts must count register pressure in hard registers, the units of
   target_avail_regs.  All the values in the loop below are 64-bit: two IVs,
   two invariants and the reduction variable r.  On ia32 and amdgcn, whose
   general registers are 32-bit, each one needs two registers.  On ia32 the
   loop then needs 10 of the 7 available and the register-pressure cost is
   not 0; counting values (5) would make it 0.  On other LP64 targets each
   value fits in one register and both counts agree.  */

/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-vectorize -fdump-tree-ivopts-details" } */

long long
f (long long n, long long s, long long t)
{
  long long r = 0;
  for (long long i = 0; i < n; i++)
    r ^= i * s + t;
  return r;
}

/* { dg-final { scan-tree-dump "regs_used 2 \\(1 values\\)" "ivopts" { target { ia32 || amdgcn-*-* } } } } */
/* { dg-final { scan-tree-dump "regs: \[0-9\]+ \\(\[0-9\]+ values\\) cands" "ivopts" { target { ia32 || amdgcn-*-* } } } } */
/* { dg-final { scan-tree-dump-not "regs: (\[1-9\]\[0-9\]*) \\(\\1 values\\) cands" "ivopts" { target { ia32 || amdgcn-*-* } } } } */
/* { dg-final { scan-tree-dump-not ", (\[1-9\]\[0-9\]*) \\(\\1 values\\) invs" "ivopts" { target { ia32 || amdgcn-*-* } } } } */
/* { dg-final { scan-tree-dump "reg_cost: \[1-9\]" "ivopts" { target ia32 } } } */

/* { dg-final { scan-tree-dump "regs_used 1 \\(1 values\\)" "ivopts" { target { lp64 && { ! amdgcn-*-* } } } } } */
/* { dg-final { scan-tree-dump "regs: (\[1-9\]\[0-9\]*) \\(\\1 values\\) cands, (\[1-9\]\[0-9\]*) \\(\\2 values\\) invs, 1 used" "ivopts" { target { lp64 && { ! amdgcn-*-* } } } } } */
