/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-options "-O2" } */

/* { dg-final { scan-assembler-not {\mld} } } */
/* { dg-final { scan-assembler-not {\mlwz} } } */
/* { dg-final { scan-assembler-times {\mlbz} 2 } } */

struct mmu_gather {
        long end;
        int fullmm : 1;
};

void __tlb_reset_range(struct mmu_gather *p1)
{
        if (p1->fullmm)
                p1->end = 0;
}

void tlb_gather_mmu(struct mmu_gather *p1)
{
        p1->fullmm = 1;
        __tlb_reset_range(p1);
}
