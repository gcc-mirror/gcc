/* Ensure dllexport overrides dllimport.  */

/* { dg-do compile { target arm*-*-pe* } } */
/* { dg-do compile { target thumb*-*-pe* } } */

__declspec (dllimport) int foo1 ();
__declspec (dllexport) int foo1 ();

__declspec (dllexport) int foo2 ();
__declspec (dllimport) int foo2 ();

__declspec (dllexport) int foo1 () { return foo2 (); }
__declspec (dllexport) int foo2 () { return foo1 (); }

/* { dg-final { scan-assembler "\.section\[ \t\]*\.drectve\n\[^\n\]*-export:foo1.*\.section\[ \t\]*\.drectve\n\[^\n\]*-export:foo2" } } */
/* { dg-final { scan-assembler-not "(__imp_foo1|__imp_foo2)" } } */
