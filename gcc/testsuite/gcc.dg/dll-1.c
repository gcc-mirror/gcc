/* { dg-do compile { target arm*-*-pe* } } */
/* { dg-options -mno-nop-fun-dllimport } */

__declspec (dllimport) void imp ();

__declspec (dllexport) void _exp () { imp (); }

/* { dg-final { scan-assembler "\.section\[ \t\]*.drectve\n\[^\n\]*-export:_exp.*__imp_imp" } } */
/* { dg-final { scan-assembler-not "__imp__exp" } } */

