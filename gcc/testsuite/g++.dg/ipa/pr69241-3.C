// PR ipa/69241
// { dg-do compile }
// { dg-options "-O2" }

struct R { int x[100]; };
__attribute__ ((noreturn)) R bar ();

void
foo ()
{
  bar ();
}
