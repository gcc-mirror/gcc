// { dg-do assemble  }
//Inheritance from templates which are namespace members
namespace foo {

  template <class T>
  struct x {
    x(){}
  };

}

class y : public foo::x<int> {};

y r;
