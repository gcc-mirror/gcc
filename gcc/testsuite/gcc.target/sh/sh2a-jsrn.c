/* Testcase to check generation of a SH2A specific instruction for
   'JSR/N @Rm'.  */
/* { dg-do assemble }  */
/* { dg-options "-O0" }  */
/* { dg-skip-if "" { "sh*-*-*" } "*" "-m2a -m2a-nofpu -m2a-single -m2a-single-only" }  */
/* { dg-final { scan-assembler "jsr/n"} }  */

void foo(void)
{
}

void bar()
{
  foo();
}
