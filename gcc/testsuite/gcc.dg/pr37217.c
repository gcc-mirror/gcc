/* PR 37217 ICE with -Wconversion */
/* { dg-do compile } */
/* { dg-options "-Wconversion" } */
typedef struct Tcl_ResolvedVarInfo {
  char *re_guts;
} regex_t;
void TclReComp(regex_t *re)
{
  if (re->re_guts == ((void *)0)) ;
}
