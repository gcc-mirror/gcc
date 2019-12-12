// PR c++/85864
// { dg-do compile { target c++14 } }
// { dg-options -w }

template<class T, T... S> struct String_template {};

template<class C, C... S>
constexpr String_template<C, S...> operator""_template() {
    return String_template<C, S...> {};
}

template<class prefix = decltype("0x"_template), class T>
int hex(T v) { return 1; }

template<int v> 
void tt2() {
  //    auto h2 = hex<decltype("0x"_template)>(1);
    auto h = hex(2);
}

int main() {
  //    auto h = hex(2);
  //    return h;
}
