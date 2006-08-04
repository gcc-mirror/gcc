// PR c++/28148

struct foo {
public:
  virtual int bar(int);
};

void (foo::*__Virtual__foo__Var1)() = (void (foo::*)())(&foo::bar);
