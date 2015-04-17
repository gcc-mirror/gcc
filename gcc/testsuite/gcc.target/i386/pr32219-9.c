/* { dg-do compile { target *-*-linux* } } */
/* { dg-require-effective-target pie_copyreloc } */
/* { dg-options "-O2 -fpie" } */

/* Uninitialized common symbol with -fpie.  */
int xxx;

int
foo ()
{
  return xxx;
}

/* { dg-final { scan-assembler "movl\[ \t\]xxx\\(%rip\\), %eax" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-not "xxx@GOTPCREL" { target { ! ia32 } } } } */
