/* { dg-do compile { target arm*-*-pe* } } */
/* { dg-options -mno-nop-fun-dllimport } */

__declspec (dllimport) void imp ();

__declspec (dllexport) void exp () { imp (); }

/* { dg-final { scan-assembler dll-1.c "__imp_imp.*\.section\[ \t\]*.drectve\n\[^\n\]*-export:exp" } } */
/* { dg-final { scan-assembler-not dll-1.c "__imp_exp" } } */
