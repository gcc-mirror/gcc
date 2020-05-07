// PR c++/90448
// { dg-do compile { target c++14 } }

template<class ... Ts> int fooV(Ts ... ts) {
  auto L = [](auto ... a) {
    auto M = [](decltype(a) ... b) -> void {
    };
    return M;
  };

  decltype(L(L, ts...)) (*fp)(decltype(L), decltype(ts) ...) = L;

  return 0;
}

int run2 = fooV("BC", 3, 2.77, 'A', float{}, short{}, unsigned{});
