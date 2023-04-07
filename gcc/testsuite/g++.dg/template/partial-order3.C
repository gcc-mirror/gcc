// PR c++/108390
// { dg-do compile { target c++11 } }

template<class T, T t>  long f(int(*)[t], T(*)[t]);
template<class T, int i> int f(int(*)[i], T(*)[i]) = delete;
int n = f<int, 2>(0, 0);
