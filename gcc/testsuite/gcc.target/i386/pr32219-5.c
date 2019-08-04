/* { dg-do compile } */
/* { dg-require-effective-target pie } */
/* { dg-options "-O2 -fpie" } */

/* Initialized symbol with -fpie.  */
int xxx = -1;

int
foo ()
{
  return xxx;
}

/* { dg-final { scan-assembler "movl\[ \t\]_?xxx\\(%rip\\), %eax" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-not "xxx@GOTPCREL" { target { ! ia32 } } } } */

/* { dg-final { scan-assembler "movl\[ \t\]xxx@GOTOFF\\(%\[^,\]*\\), %eax" { target { ia32 && { ! *-*-darwin* } } } } } */
/* { dg-final { scan-assembler-not "movl\[ \t\]xxx@GOT\\(%\[^,\]*\\), %eax" { target { ia32 && { ! *-*-darwin* } } } } } */

/* For Darwin m32, we need PIC (the default) to allow PIE.  */
/* { dg-final { scan-assembler {movl[ \t]_xxx-L1\$pb\(%eax\),[ \t]%eax} { target { ia32 && *-*-darwin* } } } } */
