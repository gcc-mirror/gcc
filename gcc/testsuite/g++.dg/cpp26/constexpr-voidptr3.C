// CWG 2819 - Cast from null pointer value in a constant expression
// { dg-do compile { target c++26 } }

struct S { int s; };

constexpr S *
foo ()
{
  void *p = nullptr;
  return static_cast<S *> (p);
}

static_assert (foo () == nullptr);
