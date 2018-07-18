// PR c++/81236
// { dg-do compile { target c++14 } }

struct A { constexpr operator int() { return 24; } };

struct MyType {
  void crash() {
    auto l = [&](auto i){
      MyType::make_crash<i>(); // Line (1)
    };

    l(A{});
  }
    
  template<int i>
  void make_crash() {}
};
