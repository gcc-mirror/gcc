/* { dg-options "-O2 isa>=3" } */

NOMIPS16 void
f1 (int *area)
{
  __builtin_mips_cache (20, area);
}

NOMIPS16 void
f2 (const short *area)
{
  __builtin_mips_cache (24, area + 10);
}

NOMIPS16 void
f3 (volatile unsigned int *area, int offset)
{
  __builtin_mips_cache (0, area + offset);
}

NOMIPS16 void
f4 (const volatile unsigned char *area)
{
  __builtin_mips_cache (4, area - 80);
}

/* { dg-final { scan-assembler "\tcache\t0x14,0\\(\\\$4\\)" } } */
/* { dg-final { scan-assembler "\tcache\t0x18,20\\(\\\$4\\)" } } */
/* { dg-final { scan-assembler "\tcache\t0x0,0\\(\\\$.\\)" } } */
/* { dg-final { scan-assembler "\tcache\t0x4,-80\\(\\\$4\\)" } } */
