// { dg-do compile }
// Origin: Giovanni Bajo <giovannibajo at gcc dot gnu dot org>
// DR161: Access to protected nested type 

namespace N1 {
  struct A 
  {
  protected:
    typedef int type;
  };

  struct B : public A
  {
    void test(void)
    {
      A::type t;
    }

    friend void ftest(void)
    {
      A::type t;
    }
  };
}


namespace N2 {
  template <class T>
  struct A 
  {
  protected:
    typedef int type;
  };

  template <class T>
  struct B : public A<T>
  {
    void test(B b)
    {
      typename A<T>::type t;
    }

    friend void ftest(B b)
    {
      typename A<T>::type t;
    }
  };

  template struct B<void>;
}
