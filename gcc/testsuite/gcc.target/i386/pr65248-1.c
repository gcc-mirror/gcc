/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -fpic" } */

/* Common symbol with -fpic.  */
__attribute__((visibility("protected")))
int xxx;

int
foo ()
{
  return xxx;
}

/* { dg-final { scan-assembler-not "xxx\\(%rip\\)" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "xxx@GOTPCREL" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-not "xxx@GOTOFF" { target ia32 } } } */
/* { dg-final { scan-assembler "xxx@GOT\\(" { target ia32 } } } */
