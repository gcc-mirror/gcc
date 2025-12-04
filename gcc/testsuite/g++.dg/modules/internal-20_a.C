// PR c++/122994
// { dg-additional-options "-fmodules -Wtemplate-names-tu-local" }
// { dg-module-cmi m }

export module m;

extern "C++" {
  static int internal() { return 42; }
}

export int a = internal();
export int b = []{ return internal(); }();

export template <typename T> int c
  = []{ return internal(); }();  // { dg-warning "refers to TU-local entity" }
export template <typename T> int d
  = []{ return internal(); }();  // { dg-warning "refers to TU-local entity" }
template int d<int>;

export int e = []{
  return []{
    return internal();
  }();
}();

export int f = []{
  struct S {
    inline int foo() {
      return internal();
    }
  };
  return S{}.foo();
}();

export extern "C++" {
  int merge = []{ return internal(); }();
}
