/* { dg-do compile } */
/* { dg-require-effective-target pie } */
/* { dg-options "-O2 -fpic" } */

/* Initialized symbol with -fpic.  */
int xxx = -1;

int
foo ()
{
  return xxx;
}

/* { dg-final { scan-assembler-not "movl\[ \t\]xxx\\(%rip\\), %" { target { { ! ia32 } && { ! *-*-darwin* } } } } } */
/* { dg-final { scan-assembler "xxx@GOTPCREL" { target { { ! ia32 } && { ! *-*-darwin* } } } } } */

/* Darwin m64 is always PIC, and the dynamic linker doesn't need an indirection.  */
/* { dg-final { scan-assembler {movl[ \t]_xxx\(%rip\),[ \t]%eax} { target { { ! ia32 } && *-*-darwin* } } } } */

/* { dg-final { scan-assembler-not "movl\[ \t\]xxx@GOTOFF\\(%\[^,\]*\\), %" { target { ia32 && { ! *-*-darwin* } } } } } */
/* { dg-final { scan-assembler "movl\[ \t\]xxx@GOT\\(%\[^,\]*\\), %" { target { ia32 && { ! *-*-darwin* } } } } } */

/* Darwin m32 defaults to PIC, so no change.  */
/* { dg-final { scan-assembler {movl[ \t]_xxx-L1\$pb\(%eax\),[ \t]%eax} { target { ia32 && *-*-darwin* } } } } */
