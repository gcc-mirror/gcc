/* PR c/8518 */
/* Contributed by Volker Reichelt. */

/* Verify that GCC doesn't get confused by the
   redefinition of an extern inline function. */

/* { dg-options "-fgnu89-inline" } */

extern int inline foo () { return 0; }
extern int inline bar () { return 0; }
static int bar () { return foo(); }
