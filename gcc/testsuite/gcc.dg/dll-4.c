/* { dg-do compile { target arm*-*-pe* } } */
/* { dg-do compile { target i?86-pc-cygwin } } */
/* { dg-do compile { target i?86-pc-mingw* } } */

__declspec (dllimport) int foo1;
int foo1;	/* { dg-warning "redeclared without dllimport" } */

__declspec (dllimport) int foo2;
int foo2 = 5;	/* { dg-warning "redeclared without dllimport" } */

int f () { return foo1 + foo2; }

/* FIXME: We should scan the output of nm for this case.  */
/* { dg-final { scan-assembler "(foo2:.*\.comm\[ \t_\]*foo1)" } } */
/* { dg-final { scan-assembler-not "(__imp_|_imp__)" } } */
