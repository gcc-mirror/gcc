// PR c++/47704
// { dg-do compile { target c++11 } }

void
foo ()
{
  enum class E { A, B };
  new E;
}
