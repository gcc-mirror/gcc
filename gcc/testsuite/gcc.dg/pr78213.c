/* { dg-do compile } */
/* { dg-options "-fself-test=$srcdir/selftests" } */

/* Verify that -fself-test does not fail on a non empty source.  */

int i;                                                                          void bar();                                                                     void foo()
{
  while (i--)
    bar();
}

/* { dg-regexp {^-fself-test: [0-9]+ pass\(es\) in [.0-9]+ seconds$|.*: note: self-tests are not enabled in this build$} } */
