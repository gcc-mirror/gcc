// PR c++/90583
// DR 1722: Lambda to function pointer conversion should be noexcept.
// { dg-do compile { target c++11 } }

void
foo ()
{
  auto l = [](int){ return 42; };
  static_assert(noexcept((int (*)(int))(l)), "");
}
