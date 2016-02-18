// PR ipa/69241
// { dg-do compile }
// { dg-options "-O2" }

__attribute__((noreturn)) void foo (int);
struct R { R (const R &) {} };

R
bar ()
{
  foo (0);
}

R
baz ()
{
  foo (0);
}
