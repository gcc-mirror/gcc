/* These dllimport and dllexport appearing for a symbol.
   The desired behavior is that if both dllimport
   and dllexport appear (in either order) the result is dllexport.

   Microsoft's MSVC 2.0 allows dllimport followed by dllexport for variables,
   but does not allow dllexport followed by dllimport.

   In C, it's ok to redeclare a variable so this works for variables
   and functions.  In C++, it only works for functions.  */

/* { dg-require-dll "" } */

__declspec (dllimport) int foo1 ();
__declspec (dllexport) int foo1 ();

__declspec (dllexport) int foo2 ();
__declspec (dllimport) int foo2 ();

__declspec (dllimport) int bar1;
__declspec (dllexport) int bar1;

__declspec (dllexport) int bar2;
__declspec (dllimport) int bar2;
