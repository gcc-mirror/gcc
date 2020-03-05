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
/* For Darwin m64 we are always PIC, but common symbols are indirected, which happens to
   match the general "ELF" case.  */
/* { dg-final { scan-assembler "xxx@GOTPCREL" { target { ! ia32 } } } } */

/* { dg-final { scan-assembler-not "movl\[ \t\]xxx@GOTOFF\\(%\[^,\]*\\), %" { target { ia32 && { ! *-*-darwin* } } } } } */
/* { dg-final { scan-assembler "movl\[ \t\]xxx@GOT\\(%\[^,\]*\\), %" { target { ia32 && { ! *-*-darwin* } } } } } */

/* Darwin m32 defaults to PIC but common symbols need to be indirected.  */
/* { dg-final { scan-assembler {movl[ \t]l_xxx\$non_lazy_ptr-L1\$pb\(%eax\),[ \t]%eax} { target { ia32 && *-*-darwin* } } } } */

