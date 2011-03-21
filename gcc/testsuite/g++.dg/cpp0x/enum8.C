// PR c++/47704
// { dg-do compile }
// { dg-options "-std=c++0x" }

void
foo ()
{
  enum class E { A, B };
  new E;
}
