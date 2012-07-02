/* Check that the bswap32 pattern is generated as swap.b and swap.w
   instructions.  */
/* { dg-do compile { target "sh*-*-*" } } */
/* { dg-options "-O2" } */
/* { dg-skip-if "" { "sh*-*-*" } { "-m5*"} { "" } }  */
/* { dg-final { scan-assembler-times "swap.w" 7 } } */
/* { dg-final { scan-assembler-times "swap.b" 16 } } */
/* { dg-final { scan-assembler-times "extu.w" 2 } } */
/* { dg-final { scan-assembler-times "mov" 1 } } */
/* { dg-final { scan-assembler-not "{shll8|shlr8|shld|shad}" } } */

int
test_func_00 (int a)
{
  /* 1x swap.w
     2x swap.b  */
  return __builtin_bswap32 (a);
}

unsigned int
test_func_01 (unsigned int a)
{
  /* 1x swap.w
     2x swap.b  */
  return __builtin_bswap32 (a);
}

int
test_func_02 (int a)
{
  /* 1x swap.w
     2x swap.b  */
  return (((a >> 0) & 0xFF) << 24)
	 | (((a >> 8) & 0xFF) << 16)
	 | (((a >> 16) & 0xFF) << 8)
	 | (((a >> 24) & 0xFF) << 0);
}

unsigned int
test_func_03 (unsigned int a)
{
  /* 1x swap.w
     2x swap.b  */
  return (((a >> 0) & 0xFF) << 24)
	 | (((a >> 8) & 0xFF) << 16)
	 | (((a >> 16) & 0xFF) << 8)
	 | (((a >> 24) & 0xFF) << 0);
}

int
test_func_04 (int a)
{
  /* 1x swap.b
     1x extu.w  */
  return __builtin_bswap32 (a) >> 16;
}

unsigned short
test_func_05 (unsigned short a)
{
  /* 1x swap.b
     1x extu.w  */
  return __builtin_bswap32 (a) >> 16;
}

long long
test_func_06 (long long a)
{
  /* 2x swap.w
     4x swap.b  */
  return __builtin_bswap64 (a);
}

long long
test_func_07 (long long a)
{
  /* 1x swap.w
     2x swap.b
     1x mov #0,Rn  */
  return __builtin_bswap64 (a) >> 32;
}

