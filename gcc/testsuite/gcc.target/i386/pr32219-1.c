/* { dg-do compile } */
/* { dg-require-effective-target pie } */
/* { dg-options "-O2 -fpie" } */

/* Initialized common symbol with -fpie.  */
int xxx = 5;
int xxx;

int
foo ()
{
  return xxx;
}

/* { dg-final { scan-assembler {movl[ \t]_?xxx\(%rip\),[ \t]%eax} { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-not "_?xxx@GOTPCREL" { target { ! ia32 } } } } */

/* { dg-final { scan-assembler "movl\[ \t\]xxx@GOTOFF\\(%\[^,\]*\\), %eax" { target { ia32 && { ! *-*-darwin* } } } } } */
/* { dg-final { scan-assembler-not "movl\[ \t\]_?xxx@GOT\\(%\[^,\]*\\), %eax" { target { ia32 && { ! *-*-darwin* } } } } } */

/* For Darwin, we default to PIC - but that's needed for Darwin's PIE.  */
/* { dg-final { scan-assembler {movl[ \t]_xxx-L1\$pb\(%eax\),[ \t]%eax} { target { ia32 && *-*-darwin* } } } } */
