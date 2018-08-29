// { dg-do assemble  }
// { dg-options "-Wno-deprecated -Wno-return-type" }
// prms-id: 13417

class   Foo {
public:
  explicit Foo (int){}
};
Foo f(10);
Foo blat() return f(4){} // { dg-error "" } named return value
 
