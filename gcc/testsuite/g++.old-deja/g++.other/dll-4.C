// Build don't link:
// Skip if not target: arm-*pe
// Ensure dllexport overrides dllimport.
// set compiler_result "\.section${spaces}\.drectve\n\[^\n\]*-export:foo1.*\.section${spaces}\.drectve\n\[^\n\]*-export:foo2"
// set not_compiler_result "(__imp_foo1|__imp_foo2)"

__declspec (dllimport) int foo1 ();
__declspec (dllexport) int foo1 ();

__declspec (dllexport) int foo2 ();
__declspec (dllimport) int foo2 ();

__declspec (dllexport) int foo1 () { return foo2 (); }
__declspec (dllexport) int foo2 () { return foo1 (); }
