// PR c++/22137

struct A
{
  static void a1();
  template <typename T>
  static void b1(T);
  template <int I>
  struct B {
    static void b1();
    template <typename T>
    static void b2(T);
  };
  struct C {
    static void c1();
  };
};

template<int I> void f1()
{
  A* p;
  A::template a1(); // { dg-error "template" }
  A::template b1(0);
  p->template a1(); // { dg-error "template" }
  p->template b1('a');

  A::template B<0>::b1();
  A::template B<0>::template b1(); // { dg-error "template" }
  A::template B<0>::template b2(0);
  A::template B<0>::template b2<double>(0);

  // Because B<I> is dependent, none of these are errors, as this
  // function is not instantiated. 
  A::template B<I>::b1();
  A::template B<I>::template b1();
  A::template B<I>::template b2(0);
  A::template B<I>::template b2<double>(0);

  A::template C::c1(); // { dg-error "template" }
}

template<int I> void f2()
{
  // These are copies of lines from f1, but this function is
  // instantiated, so we should get errors here.
  A::template B<I>::b1();
  A::template B<I>::template b1(); // { dg-error "template" }
  A::template B<I>::template b2(0);
  A::template B<I>::template b2<double>(0);
}

template void f2<0>(); // { dg-error "instantiated" }
