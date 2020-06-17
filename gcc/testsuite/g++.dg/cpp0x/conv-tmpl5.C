// PR c++/94068 - ICE with template codes in check_narrowing.
// { dg-do compile { target c++11 } }

enum class A { A1, A2 };
A foo ();
long foo (int);

template <typename>
void
bar ()
{
  const auto c{foo ()};
}
