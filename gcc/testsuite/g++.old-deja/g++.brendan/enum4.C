// { dg-do assemble  }
// GROUPS passed enums
class X {
public:
  enum { a };
};

enum { b = 1 };
enum ok {  y = b };
enum notok { z = X::a };
