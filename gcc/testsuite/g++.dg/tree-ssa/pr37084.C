// PR tree-optimization/37084
// { dg-do compile }
// { dg-options "-O" }

struct A
{
  A ();
};

inline A
foo ()
{
  return A ();
}

const A a (foo ());
