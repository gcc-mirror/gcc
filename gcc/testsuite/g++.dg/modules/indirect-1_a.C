// { dg-module-do run }

export module foo;
// { dg-module-bmi foo }

namespace foo {

  export int frob (int i)
  {
    return i;
  }


  export class X 
  {
    int i;

  public:
    X (int i) :i(i) { }
    operator int () const { return i; }
  };
}
