// { dg-do assemble  }
//In the base class list, the context of the current is used
//reported by Stephen Vavasis <vavasis@CS.Cornell.EDU>

namespace N1 {
  namespace N2 {
    class A{};
    class B;
  }
}

class N1::N2::B : public A {
};


class C1 {
    class A{};
    class B;
};

class C1::B : A {
};
