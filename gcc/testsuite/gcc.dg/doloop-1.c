/* Make sure both loops are recognized as doloops.
   If so, "bdnz" will be generated on ppc; if not,
   you will get "ble". */

/* { dg-do compile { target powerpc-*-* } } */
/* { dg-options "-O2" } */
void foo (int count, char* pca, char* pcb) {
    int i;
    if (count > 10)
        for (i = 0; i < count; ++i)
            pcb += i;
    else
        for (i = 0; i < count; ++i)
            pca += i;
    *pca = *pcb;
}
/* { dg-final { scan-assembler "bdnz" } } */
/* { dg-final { scan-assembler-not "blt" } } */
