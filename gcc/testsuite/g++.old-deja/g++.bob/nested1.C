// Build don't link: 
  class A {
    class B {
      typedef long T;
      int i;
    };
  };
  class C {
    class B {
      typedef float T;
      int i;
    };
  };

C::B::T a;
