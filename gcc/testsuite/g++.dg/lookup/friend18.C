// PR c++/70522

namespace A {
  struct C {
    friend void i();
  };
  namespace {
    int i;
  }
}

int main()
{
  return A::i;
}
