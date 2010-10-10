/* Ensure dllexport overrides dllimport.  */

/* { dg-do compile { target arm*-*-pe* } } */
/* { dg-do compile { target i?86-pc-cygwin } } */
/* { dg-do compile { target i?86-*-mingw* x86_64-*-mingw*} } */

__declspec (dllimport) int foo1 ();
__declspec (dllexport) int foo1 ();	/* { dg-warning "previous dllimport ignored" } */

__declspec (dllexport) int foo2 ();
__declspec (dllimport) int foo2 ();	/* { dg-warning "dllimport ignored" } */

__declspec (dllexport) int foo1 () { return foo2 (); }
__declspec (dllexport) int foo2 () { return foo1 (); }

/* { dg-final { scan-assembler "\.section\[ \t\]*.drectve\n.*-export:\[\\\\\"\]*foo2" } } */
/* { dg-final { scan-assembler "-export:\[\\\\\"\]*foo1" } } */
/* { dg-final { scan-assembler-not "(__imp_foo1|_imp__foo1|__imp_foo2|_imp__foo2)" } } */
