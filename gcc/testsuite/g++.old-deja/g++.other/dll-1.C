// { dg-do assemble { target arm-*-*pe } }
// { dg-options "-mno-nop-fun-dllimport" }
// declspec test #1
// set compiler_result "__imp_imp.*\.section${spaces}.drectve\n\[^\n\]*-export:exp"
// set not_compiler_result "__imp_exp"

__declspec (dllimport) void imp ();

__declspec (dllexport) void exp () { imp (); }
