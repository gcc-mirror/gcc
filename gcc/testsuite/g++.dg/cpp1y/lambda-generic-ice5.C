// PR c++/78337
// { dg-do compile { target c++14 } }

struct X {
  static constexpr int foo (int b) {
    return b;
  }
};

template<int>
using Void = void;

template<typename F,typename A>
auto
bar(F f, A a) -> decltype( ( f(a) , 0 ) ) // { dg-error "no match" }
{ return {}; }


int main() {
  //constexpr
  int f = 3;
  (void)f;
  auto l = [](auto of_type_X)->
    Void<(decltype(of_type_X)::foo(f), 0)> // { dg-error "variable" }
    {return;};
  bar(l , X{});			// { dg-error "no match" }
}
