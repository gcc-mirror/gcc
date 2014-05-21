// PR c++/51640

class ex {};

namespace t
{
  class ex2 : public ex {};
}

class ex2 : public ex {};

void bar()
{
  using namespace t;

  try {
  } catch (ex2&) { // { dg-error "reference to 'ex2' is ambiguous" }
  }
}
