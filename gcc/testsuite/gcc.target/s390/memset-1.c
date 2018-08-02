/* Make sure that short memset's with constant length are emitted
   without loop statements.  */

/* { dg-do compile } */
/* { dg-options "-O3 -mzarch -march=z13" } */

/* 1 stc */
void
*memset0(void *s, int c)
{
  return __builtin_memset (s, c, 1);
}

/* 1 stc 1 mvc */
void
*memset1(void *s, int c)
{
  return __builtin_memset (s, c, 42);
}

/* 3 stc 3 mvc */
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

/* 1 stc 1 mvc */
void
*memset4(void *s, int c)
{
  return __builtin_memset (s, c, 256);
}

/* 2 stc 2 mvc */
void
*memset5(void *s, int c)
{
  return __builtin_memset (s, c, 512);
}

/* 2 stc 2 mvc - still due to the stc bytes */
void
*memset6(void *s, int c)
{
  return __builtin_memset (s, c, 514);
}

/* 3 stc 2 mvc */
void
*memset7(void *s, int c)
{
  return __builtin_memset (s, c, 515);
}

/* 4 stc 4 mvc - 4 * 256 + 4 stc bytes */
void
*memset8(void *s, int c)
{
  return __builtin_memset (s, c, 1028);
}

/* 2 stc 1 pfd 2 mvc - start using mvc loop */
void
*memset9(void *s, int c)
{
  return __builtin_memset (s, c, 1029);
}

/* 2 stc 1 stcy 3 mvc - displacement overflow after the first */
void
*memset10(void *s, int c)
{
  return __builtin_memset ((char*)s + 4000, c, 700);
}

/* 1 mvi */
void
*clrmem0(void *s)
{
  return __builtin_memset (s, 0, 1);
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

/* 4 xc */
void
*clrmem6(void *s)
{
  return __builtin_memset (s, 0, 1024);
}

/* 2 xc - start using xc loop*/
void
*clrmem7(void *s)
{
  return __builtin_memset (s, 0, 1025);
}

/* 5 xc - on z10 PFD would be used in the loop body so the unrolled
   variant would still be shorter.  */
__attribute__ ((target("tune=z10")))
void
*clrmem7_z10(void *s)
{
  return __builtin_memset (s, 0, 1025);
}

/* 5 xc */
__attribute__ ((target("tune=z10")))
void
*clrmem8_z10(void *s)
{
  return __builtin_memset (s, 0, 1280);
}

/* 1 pfd 2 xc - start using xc loop also on z10 */
__attribute__ ((target("tune=z10")))
void
*clrmem9_z10(void *s)
{
  return __builtin_memset (s, 0, 1281);
}

/* 3 xc - displacement overflow after the first */
void
*clrmem10(void *s)
{
  return __builtin_memset (s + 4000, 0, 700);
}

/* { dg-final { scan-assembler-times "mvi\\s" 1 } } */
/* { dg-final { scan-assembler-times "mvc\\s" 20 } } */
/* { dg-final { scan-assembler-times "xc\\s" 28 } } */
/* { dg-final { scan-assembler-times "stc\\s" 21 } } */
/* { dg-final { scan-assembler-times "stcy\\s" 1 } } */
/* { dg-final { scan-assembler-times "pfd\\s" 2 } } */
