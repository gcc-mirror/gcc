// { dg-do assemble  }
// Origin: Matt Austern <austern@isolde.engr.sgi.com>

class A {
public:
  static void f();
};

template <class T>
class B : public A {
  friend void A::f();
};

template <class T>
class C : public B<T>
{
};

template class C<char>;
