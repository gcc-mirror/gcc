// { dg-do run }
// Origin: Giovanni Bajo <giovannibajo at gcc dot gnu dot org>
// DR193: Order of destruction of local automatics of destructor 

extern "C" void abort(void);

namespace N1 {
  bool a_done = false;
  struct A
  { 
    ~A()
    {
      a_done = true;
    }
  };

  struct B
  { 
    ~B()
    {
      if (!a_done)
        abort();
    }
  };

  struct C {
    B x;
    ~C() {
      A y;
    };
  };
}


namespace N2 {
  bool a_done = false;

  template <class>
  struct A
  { 
    ~A()
    {
      a_done = true;
    }
  };

  template <class>
  struct B
  { 
    ~B()
    {
      if (!a_done)
        abort();
    }
  };

  template <class T>
  struct C {
    B<T> x;
    ~C() {
      A<T> y;
    };
  };
}


int main(void)
{
  N1::C c1;
  N2::C<void> c2;
  return 0;
}
