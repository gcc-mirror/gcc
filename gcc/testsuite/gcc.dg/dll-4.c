/* { dg-do compile { target arm*-*-pe* } } */

__declspec (dllimport) int foo1;
int foo1;

__declspec (dllimport) int foo2;
int foo2 = 5;

int f () { return foo1 + foo2; }

/* FIXME: We should scan the output of nm for this case.  */
/* { dg-final { scan-assembler dll-4.c "(\.comm\[ \t\]*foo1.*\nfoo2:|\nfoo2:.*\.comm\[ \t\]*foo1)" } } */
/* { dg-final { scan-assembler-not dll-4.c "__imp_" } } */
