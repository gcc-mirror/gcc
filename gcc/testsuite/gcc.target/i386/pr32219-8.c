/* { dg-do compile } */
/* { dg-require-effective-target pie } */
/* { dg-options "-O2 -fpic" } */

/* Weak initialized symbol with -fpic.  */
__attribute__((weak))
int xxx = -1;

int
foo ()
{
  return xxx;
}

/* { dg-final { scan-assembler-not "movl\[ \t\]xxx\\(%rip\\), %eax" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "xxx@GOTPCREL" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-not "movl\[ \t\]xxx@GOTOFF\\(%\[^,\]*\\), %eax" { target ia32 } } } */
/* { dg-final { scan-assembler "movl\[ \t\]xxx@GOT\\(%\[^,\]*\\), %eax" { target ia32 } } } */
