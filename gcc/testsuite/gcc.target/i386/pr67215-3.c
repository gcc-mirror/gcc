/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -fpic -fno-plt -fdump-rtl-expand" } */

extern int bar (void);

int
foo (void)
{
  return bar ();
}

/* { dg-final { scan-rtl-dump "\\(call \\(mem:QI \\(mem/u/c:" "expand" { target { ! x32 } } } } */
/* { dg-final { scan-rtl-dump "\\(call \\(mem:QI \\(zero_extend:DI \\(mem/u/c:" "expand" { target x32 } } } */
