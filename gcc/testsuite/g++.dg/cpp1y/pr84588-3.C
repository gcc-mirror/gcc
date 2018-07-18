// { dg-do compile { target c++14 } }

struct a {
  void b() {}
  void c(void (*) () = [] {
      if (a a(int auto)JUNK)  // { dg-error "two or more data types|condition declares a function" }
      ;
  }) {}
};

struct d {
  void e() {}
  void f(void (*) () = [] {
      for (;d d(int auto)JUNK;)  // { dg-error "two or more data types|condition declares a function" }
      ;
  }) {}
};

struct g {
  void h() {}
  void i(void (*) () = [] {
      while (g g(int auto)JUNK)  // { dg-error "two or more data types|condition declares a function" }
      ;
  }) {}
};
