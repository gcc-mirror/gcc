/* This tests whether REG_ALWAYS_RETURN notes are handled
   correctly in combine.  */
/* { dg-do compile { target fpic } } */
/* { dg-options "-O2 -fpic -fprofile-arcs" } */
/* { dg-skip-if "requires unsupported run-time relocation" { spu-*-* } { "*" } { "" } } */

void test (void)
{
  fork ();
}
