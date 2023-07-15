// PR c++/110524
// { dg-do compile { target c++20 } }

template<class T>
auto f(T t) -> decltype(g<T>(t));

namespace N {
  struct A { };
  template<class T> void g(T);
};

int main() {
  f(N::A{});
}

// { dg-final { scan-assembler "_Z1fIN1N1AEEDTcl1gIT_Efp_EES2_" } }
