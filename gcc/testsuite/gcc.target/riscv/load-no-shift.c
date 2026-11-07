/* { dg-do compile } */
/* { dg-options "-O2 -mtune=generic-ooo" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

/* Only a narrow field of the loaded word is stored, so a load of the store's
   width at the adjusted offset is enough.  */

void
f_qi (unsigned char *d, const unsigned int *s)
{
  *d = *s >> 8;
}

void
f_hi_8 (unsigned short *d, const unsigned int *s)
{
  *d = *s >> 8;
}

void
f_hi_16 (unsigned short *d, const unsigned int *s)
{
  *d = *s >> 16;
}

#if __riscv_xlen == 64
void
f_si_8 (unsigned int *d, const unsigned long *s)
{
  *d = *s >> 8;
}

void
f_si_16 (unsigned int *d, const unsigned long *s)
{
  *d = *s >> 16;
}
#endif

/* Each function must be a single load, a single store and a return.  */
/* { dg-final { scan-assembler-times "\tl\[bhw\]u?\t" 3 { target { rv32 } } } } */
/* { dg-final { scan-assembler-times "\ts\[bhw\]\t" 3 { target { rv32 } } } } */
/* { dg-final { scan-assembler-times "\tret\n" 3 { target { rv32 } } } } */
/* { dg-final { scan-assembler-times "\tl\[bhw\]u?\t" 5 { target { rv64 } } } } */
/* { dg-final { scan-assembler-times "\ts\[bhw\]\t" 5 { target { rv64 } } } } */
/* { dg-final { scan-assembler-times "\tret\n" 5 { target { rv64 } } } } */
/* { dg-final { scan-assembler-not "\n\t(?!l\[bhw\]u?\t|s\[bhw\]\t|ret\n)\[a-z\]" } } */
