template <class T1, class T2>
class A {
  template <class S>
  class SubA {
    int _k;
  };
  T1 _t1;
  T2 _t2; // { dg-error "instantiated" }
};

template <class U>
class B { // { dg-error "" }
  class SubB1 {
    B _i; // { dg-error "" }
  };

  class SubB2 {
    int _j;
  };
  A<U,SubB1>::SubA<SubB2> _a; // { dg-error "" }
};


int main() {
  B<char> objB; // { dg-error "instantiated" }

  return 0;
}
