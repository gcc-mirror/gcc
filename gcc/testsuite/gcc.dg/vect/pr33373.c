/* { dg-do compile } */
/* { dg-additional-options "-Wno-shift-overflow" } */
void DOSMEM_FillIsrTable(int*isr) {
    int i;

    for (i=0; i<256; i++)
        isr[i]=(((short)((i*4) & 0xFFFF)) | (0xf000 & 0xFFFF) << 16);
}
