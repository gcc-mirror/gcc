// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

template<typename T>
concept C =
  requires (T& t) { t.~T(); };

class S1
{
  ~S1() { }
};

class S2
{
  ~S2() = delete;
};

int main()
{
  static_assert(C<S1>, ""); // { dg-error "failed" }
  static_assert(C<S2>, ""); // { dg-error "failed" }
}
