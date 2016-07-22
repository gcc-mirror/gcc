// PR c++/70641
// { dg-do compile }
// { dg-options "-O2" }

void
foo ()
{
  try { foo (); }
  catch (...) { __builtin_abort (); }
}
