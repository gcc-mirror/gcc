// { dg-do assemble  }
// PRMS Id: 4484 (bug 2)
// Bug: g++ does not grok abstract declarator syntax for method pointers.

template <class T> class A { };
void (A<int>::*p)() = (void (A<int>::*)())0; // { dg-bogus "" } abstract declarator failure
