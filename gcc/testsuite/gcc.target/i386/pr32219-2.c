/* { dg-do compile } */
/* { dg-require-effective-target pie } */
/* { dg-options "-O2 -fpic" } */

/* Common symbol with -fpic.  */
int xxx;

int
foo ()
{
  return xxx;
}

/* { dg-final { scan-assembler-not "movl\[ \t\]xxx\\(%rip\\), %" { target { ! ia32 } } } } */
/* For Darwin m64 PIC we make a direct access to this symbol.  */
/* { dg-final { scan-assembler "xxx@GOTPCREL" { target { { ! ia32 } && { ! *-*-darwin* } } } } } */

/* { dg-final { scan-assembler-not "movl\[ \t\]xxx@GOTOFF\\(%\[^,\]*\\), %" { target { ia32 && { ! *-*-darwin* } } } } } */
/* { dg-final { scan-assembler "movl\[ \t\]xxx@GOT\\(%\[^,\]*\\), %" { target { ia32 && { ! *-*-darwin* } } } } } */

/* Darwin m32 PIC requires the picbase adjustment.  */
/* { dg-final { scan-assembler {movl[ \t]_xxx-L1\$pb\(%eax\),[ \t]%eax} { target { ia32 && *-*-darwin* } } } } */

