/* { dg-do compile { target fpic } } */
/* { dg-options "-O1 -fpic" } */
extern int bar (void) __attribute__ ((__pure__));
extern char *baz;
void
foo (void)
{
  baz = (char *) bar ();
}
