// PR c++/22405

template <typename T> void foo(T &arg) { // { dg-error "declared" }
  arg+=1;
}

template <typename T> void foo(T &arg) { // { dg-error "redefinition" }
  arg+=2;
}

template void foo(float &arg); // { dg-bogus "" "" { xfail *-*-* } }
