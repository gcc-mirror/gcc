/* Verify that ipa-split is disabled following __builtin_constant_p.
   Same as pr49642-1.c, but we turn off FRE which currently masks
   the problem.  */

/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized -fno-tree-fre" } */

typedef unsigned int u32;
typedef unsigned long long u64;

static inline __attribute__((always_inline)) __attribute__((const))
int __ilog2_u32(u32 n)
{
 int bit;
 asm ("cntlzw %0,%1" : "=r" (bit) : "r" (n));
 return 31 - bit;
}


static inline __attribute__((always_inline)) __attribute__((const))
int __ilog2_u64(u64 n)
{
 int bit;
 asm ("cntlzd %0,%1" : "=r" (bit) : "r" (n));
 return 63 - bit;
}



static u64 ehca_map_vaddr(void *caddr);

struct ehca_shca {
        u32 hca_cap_mr_pgsize;
};

int ____ilog2_NaN (void);

static u64 ehca_get_max_hwpage_size(struct ehca_shca *shca)
{
 return 1UL << ( __builtin_constant_p(shca->hca_cap_mr_pgsize) ? ( (shca->hca_cap_mr_pgsize) < 1 ? ____ilog2_NaN() : (shca->hca_cap_mr_pgsize) & (1ULL << 63) ? 63 : (shca->hca_cap_mr_pgsize) & (1ULL << 62) ? 62 : (shca->hca_cap_mr_pgsize) & (1ULL << 61) ? 61 : (shca->hca_cap_mr_pgsize) & (1ULL << 60) ? 60 : (shca->hca_cap_mr_pgsize) & (1ULL << 59) ? 59 : (shca->hca_cap_mr_pgsize) & (1ULL << 58) ? 58 : (shca->hca_cap_mr_pgsize) & (1ULL << 57) ? 57 : (shca->hca_cap_mr_pgsize) & (1ULL << 56) ? 56 : (shca->hca_cap_mr_pgsize) & (1ULL << 55) ? 55 : (shca->hca_cap_mr_pgsize) & (1ULL << 54) ? 54 : (shca->hca_cap_mr_pgsize) & (1ULL << 53) ? 53 : (shca->hca_cap_mr_pgsize) & (1ULL << 52) ? 52 : (shca->hca_cap_mr_pgsize) & (1ULL << 51) ? 51 : (shca->hca_cap_mr_pgsize) & (1ULL << 50) ? 50 : (shca->hca_cap_mr_pgsize) & (1ULL << 49) ? 49 : (shca->hca_cap_mr_pgsize) & (1ULL << 48) ? 48 : (shca->hca_cap_mr_pgsize) & (1ULL << 47) ? 47 : (shca->hca_cap_mr_pgsize) & (1ULL << 46) ? 46 : (shca->hca_cap_mr_pgsize) & (1ULL << 45) ? 45 : (shca->hca_cap_mr_pgsize) & (1ULL << 44) ? 44 : (shca->hca_cap_mr_pgsize) & (1ULL << 43) ? 43 : (shca->hca_cap_mr_pgsize) & (1ULL << 42) ? 42 : (shca->hca_cap_mr_pgsize) & (1ULL << 41) ? 41 : (shca->hca_cap_mr_pgsize) & (1ULL << 40) ? 40 : (shca->hca_cap_mr_pgsize) & (1ULL << 39) ? 39 : (shca->hca_cap_mr_pgsize) & (1ULL << 38) ? 38 : (shca->hca_cap_mr_pgsize) & (1ULL << 37) ? 37 : (shca->hca_cap_mr_pgsize) & (1ULL << 36) ? 36 : (shca->hca_cap_mr_pgsize) & (1ULL << 35) ? 35 : (shca->hca_cap_mr_pgsize) & (1ULL << 34) ? 34 : (shca->hca_cap_mr_pgsize) & (1ULL << 33) ? 33 : (shca->hca_cap_mr_pgsize) & (1ULL << 32) ? 32 : (shca->hca_cap_mr_pgsize) & (1ULL << 31) ? 31 : (shca->hca_cap_mr_pgsize) & (1ULL << 30) ? 30 : (shca->hca_cap_mr_pgsize) & (1ULL << 29) ? 29 : (shca->hca_cap_mr_pgsize) & (1ULL << 28) ? 28 : (shca->hca_cap_mr_pgsize) & (1ULL << 27) ? 27 : (shca->hca_cap_mr_pgsize) & (1ULL << 26) ? 26 : (shca->hca_cap_mr_pgsize) & (1ULL << 25) ? 25 : (shca->hca_cap_mr_pgsize) & (1ULL << 24) ? 24 : (shca->hca_cap_mr_pgsize) & (1ULL << 23) ? 23 : (shca->hca_cap_mr_pgsize) & (1ULL << 22) ? 22 : (shca->hca_cap_mr_pgsize) & (1ULL << 21) ? 21 : (shca->hca_cap_mr_pgsize) & (1ULL << 20) ? 20 : (shca->hca_cap_mr_pgsize) & (1ULL << 19) ? 19 : (shca->hca_cap_mr_pgsize) & (1ULL << 18) ? 18 : (shca->hca_cap_mr_pgsize) & (1ULL << 17) ? 17 : (shca->hca_cap_mr_pgsize) & (1ULL << 16) ? 16 : (shca->hca_cap_mr_pgsize) & (1ULL << 15) ? 15 : (shca->hca_cap_mr_pgsize) & (1ULL << 14) ? 14 : (shca->hca_cap_mr_pgsize) & (1ULL << 13) ? 13 : (shca->hca_cap_mr_pgsize) & (1ULL << 12) ? 12 : (shca->hca_cap_mr_pgsize) & (1ULL << 11) ? 11 : (shca->hca_cap_mr_pgsize) & (1ULL << 10) ? 10 : (shca->hca_cap_mr_pgsize) & (1ULL << 9) ? 9 : (shca->hca_cap_mr_pgsize) & (1ULL << 8) ? 8 : (shca->hca_cap_mr_pgsize) & (1ULL << 7) ? 7 : (shca->hca_cap_mr_pgsize) & (1ULL << 6) ? 6 : (shca->hca_cap_mr_pgsize) & (1ULL << 5) ? 5 : (shca->hca_cap_mr_pgsize) & (1ULL << 4) ? 4 : (shca->hca_cap_mr_pgsize) & (1ULL << 3) ? 3 : (shca->hca_cap_mr_pgsize) & (1ULL << 2) ? 2 : (shca->hca_cap_mr_pgsize) & (1ULL << 1) ? 1 : (shca->hca_cap_mr_pgsize) & (1ULL << 0) ? 0 : ____ilog2_NaN() ) : (sizeof(shca->hca_cap_mr_pgsize) <= 4) ? __ilog2_u32(shca->hca_cap_mr_pgsize) : __ilog2_u64(shca->hca_cap_mr_pgsize) );
}

int x(struct ehca_shca *shca) {
        return ehca_get_max_hwpage_size(shca);
}

int y(struct ehca_shca *shca)
{
        return ehca_get_max_hwpage_size(shca);
}

/* { dg-final { scan-tree-dump-times "____ilog2_NaN" 0 "optimized" } } */
