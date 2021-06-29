/* CTF generation for static variables inside a function.

   In this testcase, CTF record for bstatic is NOT expected.  CTF generation
   is only carried out for variables at file-scope or global-scope.  */

/* { dg-do compile )  */
/* { dg-options "-O0 -gctf -dA" } */

/* { dg-final { scan-assembler-times "ctv_name" 0 } } */
/* { dg-final { scan-assembler-times "ascii \"bstatic.0\"\[\t \]+\[^\n\]*ctf_string" 0 } } */

int foo (int a)
{
  static int bstatic = 3;
  return a + bstatic;
}
