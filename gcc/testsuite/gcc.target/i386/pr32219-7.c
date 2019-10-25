/* { dg-do compile } */
/* { dg-require-effective-target pie } */
/* { dg-options "-O2 -fpie" } */

/* Weak initialized symbol with -fpie.  */
__attribute__((weak))
int xxx = -1;

int
foo ()
{
  return xxx;
}

/* { dg-final { scan-assembler "movl\[ \t\]xxx\\(%rip\\), %eax" { target { { ! ia32 } && { ! *-*-darwin* } } } } } */
/* { dg-final { scan-assembler-not "xxx@GOTPCREL" { target { { ! ia32 } && { ! *-*-darwin* } } } } } */

/* For Darwin m64, code is always PIC but we need to indirect through the GOT to allow
   weak symbols to be interposed.  The dynamic loader knows how to apply PIE to this.  */
/* { dg-final { scan-assembler {movq[ \t]_xxx@GOTPCREL\(%rip\),[ \t]%rax} { target { { ! ia32 } && *-*-darwin* } } } } */

/* { dg-final { scan-assembler "movl\[ \t\]xxx@GOTOFF\\(%\[^,\]*\\), %eax" { target { ia32 && { ! *-*-darwin* } } } } } */
/* { dg-final { scan-assembler-not "movl\[ \t\]xxx@GOT\\(%\[^,\]*\\), %eax" { target { ia32 && { ! *-*-darwin* } } } } } */

/* Darwin m32 equivalent (indirect and PIC).  */
/* { dg-final { scan-assembler {movl[ \t][Ll]_xxx\$non_lazy_ptr-L1\$pb\(%eax\),[ \t]%eax} { target { ia32 && *-*-darwin* } } } } */
