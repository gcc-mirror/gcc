/* { dg-do compile { target { *-*-linux* && { ! ia32 } } } } */
/* { dg-require-effective-target maybe_x32 } */
/* { dg-options "-O2 -mx32 -fpic -fno-plt -maddress-mode=long" } */

extern void bar (void);

void
foo (void)
{
  bar ();
}

/* { dg-final { scan-assembler "jmp\[ \t\]*.bar@GOTPCREL" } } */
