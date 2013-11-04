// PR c++/47704
// { dg-do compile }
// { dg-options "-std=c++11" }

void
foo ()
{
  enum class E { A, B };
  new E;
}
