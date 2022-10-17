// PR c++/102670
// { dg-do compile { target c++20 } }

namespace ns {
  struct S { };

  template<int I>
  constexpr int adl(const S &) {
    return I;
  }
}

namespace redirect {
  template<class T, int I>
  concept can_call_adl = requires(T t) {
    adl<I>(t);
  };

  template<int I>
  struct adl_fn {
    template<can_call_adl<I> T>
    constexpr decltype(auto) operator()(T t) const {
      return adl<I>(t);
    }
  };

  namespace {
    template<int I>
    constexpr inline adl_fn<I> adl{};
  }
}

int main() {
  static_assert(redirect::can_call_adl<ns::S, 3>);
  redirect::adl<3>(ns::S{});
}
