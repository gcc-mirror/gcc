/* Make sure that short memset's with constant length are emitted
   without loop statements.  */

/* { dg-do compile } */
/* { dg-options "-O3 -mzarch" } */

/* 1 mvc */
void
*memset1(void *s, int c)
{
  return __builtin_memset (s, c, 42);
}

/* 3 mvc */
void
*memset2(void *s, int c)
{
  return __builtin_memset (s, c, 700);
}

/* nop */
void
*memset3(void *s, int c)
{
  return __builtin_memset (s, c, 0);
}

/* mvc */
void
*memset4(void *s, int c)
{
  return __builtin_memset (s, c, 256);
}

/* 2 mvc */
void
*memset5(void *s, int c)
{
  return __builtin_memset (s, c, 512);
}

/* still 2 mvc through the additional first byte  */
void
*memset6(void *s, int c)
{
  return __builtin_memset (s, c, 514);
}

/* 3 mvc */
void
*memset7(void *s, int c)
{
  return __builtin_memset (s, c, 515);
}

/* still 3 mvc through the additional first byte  */
void
*memset8(void *s, int c)
{
  return __builtin_memset (s, c, 771);
}

/* Use mvc loop: 2 mvc */
void
*memset9(void *s, int c)
{
  return __builtin_memset (s, c, 772);
}

/* 3 mvc with displacement overflow after the first */
void
*memset10(void *s, int c)
{
  return __builtin_memset ((char*)s + 4000, c, 700);
}

/* 1 xc */
void
*clrmem1(void *s)
{
  return __builtin_memset (s, 0, 42);
}

/* 3 xc */
void
*clrmem2(void *s)
{
  return __builtin_memset (s, 0, 700);
}

/* nop */
void
*clrmem3(void *s)
{
  return __builtin_memset (s, 0, 0);
}

/* 1 xc */
void
*clrmem4(void *s)
{
  return __builtin_memset (s, 0, 256);
}

/* 2 xc */
void
*clrmem5(void *s)
{
  return __builtin_memset (s, 0, 512);
}

/* 3 xc */
void
*clrmem6(void *s)
{
  return __builtin_memset (s, 0, 768);
}

/* start using xc loop */
void
*clrmem7(void *s)
{
  return __builtin_memset (s, 0, 1281);
}

/* 3 xc with displacement overflow after the first */
void
*clrmem8(void *s)
{
  return __builtin_memset (s + 4000, 0, 700);
}

/* { dg-final { scan-assembler-times "mvc" 19 } } */
/* { dg-final { scan-assembler-times "xc" 15 } } */
