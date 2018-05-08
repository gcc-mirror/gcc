// { dg-do compile { target c++14 } }
// { dg-options "-w" }

struct a {
  void b() {}
  void c(auto = [] {
    if (a a(int auto){})  // { dg-error "two or more data types" }
      ;
  }) {}
};
