// PR c++/113544
// { dg-do compile { target c++14 } }

template<class T>
void f() {
  [](auto parm) {
    struct type : decltype(parm) { };
  };
}

template void f<int>();
