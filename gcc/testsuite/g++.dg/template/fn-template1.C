// PR c++/118516
// { dg-do compile }
// Like cpp2a/fn-template11.C but with blah declared.

int nonconst ();

int foo ()
{
  int blah = 20;
  return blah <
    nonconst (), nonconst ();
}
