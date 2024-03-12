/* { dg-options "-mno-abicalls -mgpopt -G8 -mabi=32 -mips32r2 -mips16 -mmips16e2" } */
/* { dg-skip-if "naming registers makes this a code quality test" { *-*-* } { "-O0" } { "" } } */

/* Test cache.  */

void
test01 (int *area)
{
  __builtin_mips_cache (20, area);
}

void
test02 (const short *area)
{
  __builtin_mips_cache (24, area + 10);
}

void
test03 (volatile unsigned int *area, int offset)
{
  __builtin_mips_cache (0, area + offset);
}

void
test04 (const volatile unsigned char *area)
{
  __builtin_mips_cache (4, area - 80);
}

/* { dg-final { scan-assembler "\tcache\t0x14,0\\(\\\$4\\)" } } */
/* { dg-final { scan-assembler "\tcache\t0x18,20\\(\\\$4\\)" } } */
/* { dg-final { scan-assembler "\tcache\t(0x|)0,0\\(\\\$.\\)" } } */
/* { dg-final { scan-assembler "\tcache\t0x4,-80\\(\\\$4\\)" } } */

