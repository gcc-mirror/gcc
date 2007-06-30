/* PR c/4076  -Wunused doesn't warn about static function only called by itself.  */
/* { dg-do compile } */
/* { dg-options "-Wunused-function" } */

static void foo (void) {} /* { dg-warning "'foo' defined but not used" } */
static void bar (void) { bar (); } /* { dg-warning "'bar' defined but not used" } */
