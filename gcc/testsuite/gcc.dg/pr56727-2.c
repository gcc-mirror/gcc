/* { dg-do compile { target fpic } } */
/* { dg-options "-O2 -fPIC" } */
/* { dg-require-alias "" } */

__attribute__((noinline, noclone))
void f (short b)
{
  __builtin_setjmp (0);  /* Prevent tailcall */
  f (0);
}

static void g (short) __attribute__ ((alias ("f")));

void h ()
{
  g (0);
}

/* { dg-final { scan-assembler "@(PLT|plt)" { target i?86-*-* x86_64-*-* } } } */
/* { dg-final { scan-assembler "@(PLT|plt)" { target { powerpc*-*-linux* && ilp32 } } } } */
/* { dg-final { scan-assembler "(bl f\n\\s*nop)|(bl f@notoc)" { target { powerpc*-*-linux* && lp64 } } } } */
