// Build don't link:
// Special g++ Options: -Wno-deprecated
// prms-id: 13417

class   Foo {
public:
  explicit Foo (int){}
};
Foo f(10);
Foo blat() return f(4){}; //this should not give an error
 
