/* { dg-do compile { target i?86-pc-cygwin } } */
/* { dg-do compile { target i?86-pc-mingw* } } */
/* { dg-do compile { target arm*-*-pe* } } */

/* { dg-options -mnop-fun-dllimport } */

/* The dllimport attribute should be ignored for functions. */
__declspec (dllimport) void dllimpfn ();

/* The dllimport attribute should not be ignored for variables. */
__declspec (dllimport) int dllimpvar;

/* The dllexport attribute should not be ignored. */
__declspec (dllexport) void dllexp ()
{
  dllimpfn ();
  dllimpvar = 0;
}

/* { dg-final { scan-assembler-not "(__imp_dllimpfn|_imp__dllimpfn)" } } */
/* { dg-final { scan-assembler "(__imp_dllimpvar|_imp__dllimpvar)" } } */
/* { dg-final { scan-assembler "\.section\[ \t\]*.drectve\n\.*-export:dllexp" } } */
