// { dg-do run  }
// PRMS Id: 5163
// Bug: g++ doesn't accept the explicit destructor call syntax for templates.

template <class T> struct A { };
A<int> a;

int main()
{
  a.~A();			// { dg-bogus "" } 
}
