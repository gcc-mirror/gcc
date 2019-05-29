/* { dg-do compile } */
/* { dg-require-effective-target pie } */
/* { dg-options "-O2 -fpic" } */

/* Weak common symbol with -fpic.  */
__attribute__((weak))
int xxx;

int
foo ()
{
  return xxx;
}

/* { dg-final { scan-assembler-not "movl\[ \t\]xxx\\(%rip\\), %" { target { ! ia32 } } } } */
/* Darwin is always PIC for PIE so no change, weak symbols need to be indirect and this
   happens to match the ELF case.  */
/* { dg-final { scan-assembler "xxx@GOTPCREL" { target { ! ia32 } } } } */

/* { dg-final { scan-assembler-not "movl\[ \t\]xxx@GOTOFF\\(%\[^,\]*\\), %" { target { ia32 && { ! *-*-darwin* } } } } } */
/* { dg-final { scan-assembler "movl\[ \t\]xxx@GOT\\(%\[^,\]*\\), %" { target { ia32 && { ! *-*-darwin* } } } } } */

/* Darwin m32 equivalent (indirect and PIC).  */
/* { dg-final { scan-assembler {movl[ \t]l_xxx\$non_lazy_ptr-L1\$pb\(%eax\),[ \t]%eax} { target { ia32 && *-*-darwin* } } } } */
