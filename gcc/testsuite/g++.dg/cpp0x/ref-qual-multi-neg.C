// { dg-require-effective-target c++11 }

class Foo
{
public:
  void bar() const && & { }  // { dg-error "multiple ref-qualifiers" }
};
