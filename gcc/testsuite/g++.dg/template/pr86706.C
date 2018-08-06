// PR c++/86706
// { dg-do compile }

class A { int b; };

template <class, typename>
class C : A { C (); static C *f; };

template <class T, typename U>
C<T, U> *C<T, U>::f;

template <class T, typename U>
C<T, U>::C ()
{
  f->b;
}
