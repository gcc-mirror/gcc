// { dg-do assemble { target arm-*-*pe } }
// Microsoft's MSVC 2.0 allows dllimport followed by dllexport for variables,
// but does not allow dllexport followed by dllimport.
//
// Switching between dll{export,import} works for functions.
// We test for that too (by ensuring no error is produced).

__declspec (dllimport) int foo1 ();
__declspec (dllexport) int foo1 ();

__declspec (dllexport) int foo2 ();
__declspec (dllimport) int foo2 ();

__declspec (dllimport) int bar1;
__declspec (dllexport) int bar1;

__declspec (dllexport) int bar2; // { dg-error "" } previously declared
__declspec (dllimport) int bar2; // { dg-error "" } redefinition
