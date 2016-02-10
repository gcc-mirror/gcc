// PR ipa/69241
// { dg-do compile }
// { dg-options "-O2" }

struct R { R (const R &) {} };
__attribute__ ((noreturn)) R bar ();

R
foo ()
{
  bar ();
}
