/* ivopts must count register pressure in hard registers: each 64-bit
   pointer invariant needs two 32-bit SGPRs.  The loop below needs about
   50 values (49 of them 64-bit), which fits in the 78 SGPRs available to a
   kernel only if values are counted, not registers.  */

/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-ivopts-details" } */

#define X4(M, k) M(k##0) M(k##1) M(k##2) M(k##3)
#define X48(M) X4(M, 0) X4(M, 1) X4(M, 2) X4(M, 3) X4(M, 4) X4(M, 5) \
		X4(M, 6) X4(M, 7) X4(M, 8) X4(M, 9) X4(M, a) X4(M, b)

#define DECL(k) double *a##k;
#define USE(k) + a##k[i]

X48 (DECL)
double *out;
int n;

/* main is compiled as a kernel, so the budget does not depend on whether
   normal functions get a smaller one.  */

int
main ()
{
  for (int i = 0; i < n; i++)
    out[i] = 0.0 X48 (USE);
  return 0;
}

/* { dg-final { scan-assembler "\\.amdhsa_kernel\\s+main" } } */
/* { dg-final { scan-tree-dump "target_avail_regs 78" "ivopts" } } */
/* { dg-final { scan-tree-dump "reg_cost: \[1-9\]" "ivopts" } } */
/* { dg-final { scan-tree-dump-not "reg_cost: 0\[^0-9\]" "ivopts" } } */
/* The single 64-bit IV takes two registers, and the 50 invariants (49
   pointers and n) take 99.  Every one-IV set seen during the search must
   count that IV as exactly two registers.  */
/* { dg-final { scan-tree-dump "regs: 2 \\(1 values\\) cands, 99 \\(50 values\\) invs" "ivopts" } } */
/* { dg-final { scan-tree-dump-not "regs: (\[013-9\]|\[0-9\]\[0-9\]+) \\(1 values\\) cands" "ivopts" } } */
