// PR c++/117153
// { dg-do compile }

template<typename T>
void f() {
  union {
    union {
      T d;
    };
  };
  (void) (d + 0);
}
template void f<double>();
