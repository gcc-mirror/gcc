/* { dg-do compile { target arm*-*-pe* } } */
/* { dg-do compile { target thumb*-*-pe* } } */

__declspec (dllimport) int foo1;
int foo1;

__declspec (dllimport) int foo2;
int foo2 = 5;

int f () { return foo1 + foo2; }

/* FIXME: We should scan the output of nm for this case.  */
/* { dg-final { scan-assembler "(foo2:.*\.comm\[ \t_\]*foo1)" } } */
/* { dg-final { scan-assembler-not "__imp_" } } */
