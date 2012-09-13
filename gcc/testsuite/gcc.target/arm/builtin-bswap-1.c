/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target arm_arch_v6_ok } */
/* { dg-add-options arm_arch_v6 } */
/* { dg-final { scan-assembler-not "orr\[ \t\]" } } */
/* { dg-final { scan-assembler-times "revsh\\t" 1 { target { arm_nothumb } } } }  */
/* { dg-final { scan-assembler-times "revshne\\t" 1 { target { arm_nothumb } } } }  */
/* { dg-final { scan-assembler-times "revsh\\t" 2 { target { ! arm_nothumb } } } }  */
/* { dg-final { scan-assembler-times "rev16\\t" 1 { target { arm_nothumb } } } }  */
/* { dg-final { scan-assembler-times "rev16ne\\t" 1 { target { arm_nothumb } } } }  */
/* { dg-final { scan-assembler-times "rev16\\t" 2 { target { ! arm_nothumb } } } }  */
/* { dg-final { scan-assembler-times "rev\\t" 2 { target { arm_nothumb } } } }  */
/* { dg-final { scan-assembler-times "revne\\t" 2 { target { arm_nothumb } } } }  */
/* { dg-final { scan-assembler-times "rev\\t" 4 { target { ! arm_nothumb } } } }  */

/* revsh */
short swaps16 (short x)
{
  return __builtin_bswap16 (x);
}

extern short foos16 (short);

/* revshne */
short swaps16_cond (short x, int y)
{
  short z = x;
  if (y)
    z = __builtin_bswap16 (x);
  return foos16 (z);
}

/* rev16 */
unsigned short swapu16 (unsigned short x)
{
  return __builtin_bswap16 (x);
}

extern unsigned short foou16 (unsigned short);

/* rev16ne */
unsigned short swapu16_cond (unsigned short x, int y)
{
  unsigned short z = x;
  if (y)
    z = __builtin_bswap16 (x);
  return foou16 (z);
}

/* rev */
int swaps32 (int x) {
  return __builtin_bswap32 (x);
}

extern int foos32 (int);

/* revne */
int swaps32_cond (int x, int y)
{
  int z = x;
  if (y)
    z = __builtin_bswap32 (x);
  return foos32 (z);
}

/* rev */
unsigned int swapu32 (unsigned int x)
{
  return __builtin_bswap32 (x);
}

extern unsigned int foou32 (unsigned int);

/* revne */
unsigned int swapsu2 (unsigned int x, int y)
{
  int z = x;
  if (y)
    z = __builtin_bswap32 (x);
  return foou32 (z);
}
