// { dg-do assemble  }
// GROUPS passed pt coredump
template<class T> class A;
void f () { A<int> *a; }
