// PR c++/89442
// { dg-do compile { target c++14 } }

template<class T, class U> bool vt;

template<class T>
void f() {
  bool a = vt<T>;       // { dg-error "wrong number" }
  bool b = vt<T, T>;
  bool c = vt<T, T, T>; // { dg-error "number" }
  bool d = vt<T, 42>;   // { dg-error "mismatch" }
}
