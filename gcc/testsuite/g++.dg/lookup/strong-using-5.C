// PR c++/33486

// { dg-options "" }

namespace A
{
  inline namespace B
  {
    struct T
    {
      struct U { };
      U f();
    };
  }

  inline namespace C
  {
    void g (T::U);
  }
}

int main()
{
  A::T t;
  g(t.f());
}
