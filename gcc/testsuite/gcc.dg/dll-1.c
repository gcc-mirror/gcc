/* { dg-do compile { target arm*-*-pe* } } */
/* { dg-do compile { target thumb*-*-pe* } } */
/* { dg-options -mno-nop-fun-dllimport } */

__declspec (dllimport) void imp ();

__declspec (dllexport) void exp () { imp (); }

/* { dg-final { scan-assembler "\.section\[ \t\]*.drectve\n\[^\n\]*-export:exp.*__imp_imp" } } */
/* { dg-final { scan-assembler-not "__imp_exp" } } */
