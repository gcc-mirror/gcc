// { dg-do compile { target c++11 } }

void f() {
  typedef decltype(__func__) T;
  T x = __func__;		// { dg-error "array" }
}
