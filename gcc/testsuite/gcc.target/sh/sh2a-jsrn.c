/* Testcase to check generation of a SH2A specific instruction for
   'JSR/N @Rm'.  */
/* { dg-do compile { target { sh2a } } }  */
/* { dg-options "-O0" }  */
/* { dg-final { scan-assembler "jsr/n"} }  */

void foo(void)
{
}

void bar()
{
  foo();
}
