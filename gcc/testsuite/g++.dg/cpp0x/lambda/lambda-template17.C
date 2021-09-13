// PR c++/97582
// { dg-do compile { target c++11 } }

struct C1 { void operator+(); };
struct C2 { void operator+(); };
struct C3 : C1, C2 {
  template <class T> void get() { [] (T x) { +x; }; } // { dg-error "ambiguous" }
};

template void C3::get<C1>(); // { dg-bogus "" }
template void C3::get<C2>(); // { dg-bogus "" }
template void C3::get<C3>(); // { dg-message "required from here" }
