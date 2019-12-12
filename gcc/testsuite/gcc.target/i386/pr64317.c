/* { dg-do compile } */
/* { dg-require-effective-target ia32 } */
/* { dg-require-effective-target pie } */
/* { dg-options "-O2 -fpie" } */
/* { dg-final { scan-assembler "addl\[ \\t\]+\[$\]_GLOBAL_OFFSET_TABLE_, %ebx" { target { ! *-*-darwin* } } } } */
/* { dg-final { scan-assembler "movl\[ \\t\]+c@GOTOFF\[(\]%ebx\[)\]" { target { ! *-*-darwin* } } } } */
/* { dg-final { scan-assembler-not "movl\[ \\t\]+\[0-9]+\[(\]%esp\[)\], %ebx" { target { ! *-*-darwin* } } } } */

/* Check PIC access to c and t1 on Darwin (PIC is default, needed for PIE).  */
/* { dg-final { scan-assembler {_c-L1\$pb\(%} { target *-*-darwin* } } } */
/* { dg-final { scan-assembler {_t1.[0-9]+-L1\$pb\(%} { target *-*-darwin* } } } */

long c = 1;

int bar();

int foo (unsigned int iters)
{
  unsigned int i;
  
  int res = 0;
  static long t1;
  
  for (i = 0; i < iters; i++)
    {
      res = bar();
      t1 = c + res;
    }
  
  return t1 + res;
}
