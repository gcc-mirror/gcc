// PR c++/98216
// PR c++/91292
// { dg-do compile { target c++20 } }

template<auto> void f() { }

template void f<-1.0f>();
template void f<-2.0f>();

template void f<-1.0>();
template void f<-2.0>();

template void f<-1.0L>();
template void f<-2.0L>();
