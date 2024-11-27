/* PR c/117745 */
/* { dg-do compile } */
/* { dg-options "" } */

static int foo (void);
void bar (void) { sizeof (int [0 ? foo () : 1); }	/* { dg-error "expected" } */
static int baz (void);
void qux (void) { sizeof (sizeof (int[baz ()])); }
