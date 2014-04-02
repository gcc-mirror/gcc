// Test for constexpr conversion in case context
// { dg-do compile { target c++11 } }

enum class E { e1, e2 };

struct A
{
  E e;
  constexpr operator E() { return e; }
  constexpr A(E e): e(e) { }
};

E e;

int main()
{
  switch (e)
    {
    case A(E::e1):
    case A(E::e2):
      ;
    }
}
