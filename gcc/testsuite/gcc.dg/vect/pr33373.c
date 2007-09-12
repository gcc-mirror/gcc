/* { dg-do compile } */
void DOSMEM_FillIsrTable(int*isr) {
    int i;

    for (i=0; i<256; i++)
        isr[i]=(((short)((i*4) & 0xFFFF)) | (0xf000 & 0xFFFF) << 16);
}
/* { dg-final { cleanup-tree-dump "vect" } } */
