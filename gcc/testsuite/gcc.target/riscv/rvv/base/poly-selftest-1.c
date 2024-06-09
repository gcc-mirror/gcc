/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O0 -fself-test=$srcdir/selftests -mrvv-vector-bits=zvl -S" } */

/* Verify that -fself-test does not fail on a non empty source.  */

int i;                                                                          void bar();                                                                     void foo()
{
  while (i--)
    bar();
}

/* { dg-regexp {^-fself-test: [0-9]+ pass\(es\) in [.0-9]+ seconds$|.*: note: self-tests are not enabled in this build$} } */
