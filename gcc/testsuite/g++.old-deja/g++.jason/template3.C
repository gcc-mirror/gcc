// { dg-do run  }
// PRMS Id: 4679
// Bug: g++ doesn't re-instantiate templates after definition is seen.

template <class T> struct A;

A<int> *a;

template <class T> struct A { T t; };

int main()
{
  if (a)
    a->t = 1;			// { dg-bogus "" } 
}
