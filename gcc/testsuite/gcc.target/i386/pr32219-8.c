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

/* { dg-final { scan-assembler-not "movl\[ \t\]xxx\\(%rip\\), %" { target { ! ia32 } } } } */
/* Darwin is always PIC so no change, weak symbols needs to be indirect and this
   happens to match the ELF case.  */
/* { dg-final { scan-assembler "_?xxx@GOTPCREL" { target { ! ia32 } } } } */

/* { dg-final { scan-assembler-not "movl\[ \t\]xxx@GOTOFF\\(%\[^,\]*\\), %" { target { ia32 && { ! *-*-darwin* } } } } } */
/* { dg-final { scan-assembler "movl\[ \t\]xxx@GOT\\(%\[^,\]*\\), %" { target { ia32 && { ! *-*-darwin* } } } } } */

/* Darwin m32 default to PIC but needs indirection for the weak symbol.  */
/* { dg-final { scan-assembler {movl[ \t][Ll]_xxx\$non_lazy_ptr-L1\$pb\(%eax\),[ \t]%eax} { target { ia32 && *-*-darwin* } } } } */
