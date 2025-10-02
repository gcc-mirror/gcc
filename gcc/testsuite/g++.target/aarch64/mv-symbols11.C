/* { dg-do compile } */
/* { dg-options "-O0" } */

__attribute__ ((target_version ("default"))) int
foo () { return 1; }

__attribute__ ((target_version ("dotprod"))) int
foo () { return 3; }

int (*test)();

void bar ()
{
  test = foo;
}

__attribute__ ((target_version ("default"))) int
foo2 ();

__attribute__ ((target_version ("dotprod"))) int
foo2 ();

void bar2 ()
{
  test = foo2;
}

/* { dg-final { scan-assembler-times "\n\tadrp\tx\[0-9\]+, _Z3foov\n" 1 } } */
/* { dg-final { scan-assembler-times "\n\tadrp\tx\[0-9\]+, _Z4foo2v\n" 1 } } */
