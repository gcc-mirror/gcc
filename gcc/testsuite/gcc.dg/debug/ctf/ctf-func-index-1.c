/* CTF function index sub-section.

   A function index sub-section in the CTF section contains the offset to the
   string name of the global function symbols.  The number of entries in the
   func info section and the func index section are always the same.

   In this testcase, 2 records in the function index section are expected.  */

/* { dg-do compile } */
/* { dg-options "-O0 -gctf -dA" } */

/* { dg-final { scan-assembler-times "funcinfo_name" 2 } } */
/* { dg-final { scan-assembler-times "funcinfo_func_type" 2 } } */
/* { dg-final { scan-assembler-times "ascii \"bar.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"foo.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */

int foo (void)
{
  return 0;
}

int bar (int a)
{
  return 33 + a;
}
