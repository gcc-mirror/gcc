// PR c++/108218
// { dg-do compile { target c++11 } }

template<class T>
void f() {
  decltype(new int[-1]) p; // { dg-error "negative" }
  decltype(new int[0-1]) q; // { dg-error "negative" }
  decltype(new int[1*-1]) r; // { dg-error "negative" }
}

decltype(new int[-1]) p; // { dg-error "negative" }
decltype(new int[0-1]) q; // { dg-error "negative" }
decltype(new int[1*-1]) r; // { dg-error "negative" }
