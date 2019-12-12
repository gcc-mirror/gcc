// PR c++/89744

namespace N1 {
  template<typename> struct A
  {
    template<typename> struct B {};
    A() { B<int> b; }
  };

  template<> template<typename>
  struct A<int>::B
  {
    virtual void foo() {}
  };

  A<int> a;
}

namespace N2 {
  template<typename> struct A
  {
    template<typename> struct B {};
    A() { B<int> b; }
  };

  template<> template<typename>
  struct A<int>::B
  {
    virtual void foo() {}
    void bar() {}
  };

  A<int> a;
}

namespace N3 {
  template<typename> struct A
  {
    template<typename> struct B {};
    A() { B<int> b; }
  };

  template<> template<typename>
  struct A<int>::B
  {
    ~B() {}
  };

  A<int> a;
}

#if __cpp_variadic_templates
namespace N4 {
  template<typename...> struct A
  {
    template<typename> struct B {};
    typedef B<int> X;
  };

  template<> template<typename>
  struct A<int>::B
  {
    typedef int Y;
  };

  A<int>::B<int> b;
}
#endif
