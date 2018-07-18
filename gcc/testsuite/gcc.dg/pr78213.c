/* { dg-do compile } */
/* { dg-options "-fself-test" } */

/* When this test was written -fself-test took no argument, but it
   has subsequently gained a mandatory argument, giving the path
   to selftest support files (within the srcdir).
   It's not clear how to provide this path sanely from
   within DejaGnu, so for now, this test is disabled.  */
/* { dg-skip-if "" { *-*-* } } */

/* Verify that -fself-test does not fail on a non empty source.  */

int i;                                                                          void bar();                                                                     void foo()
{
  while (i--)
    bar();
}
/* { dg-message "fself\-test: " "-fself-test" { target *-*-* } 0 } */
