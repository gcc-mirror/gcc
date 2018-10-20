// PR c++/82308
// { dg-do compile { target c++17 } }

template<typename, unsigned>
struct array {};

template <unsigned R>
class X {
public:
  using T = array<int, R>;

  enum class C : char { A, B };
  X(T bounds, C c = C::B) : t(bounds) {}

private:
  T t;
};

int main()
{
  array<int, 2> a;
  X    d{a};
  X<2> e{a};
}
