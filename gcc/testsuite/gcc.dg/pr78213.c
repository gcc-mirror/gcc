/* { dg-do compile } */
/* { dg-options "-fself-test=$srcdir/selftests" } */

/* Verify that -fself-test does not fail on a non empty source.  */

int i;                                                                          void bar();                                                                     void foo()
{
  while (i--)
    bar();
}
/* { dg-message "fself\-test: " "-fself-test" { target *-*-* } 0 } */
