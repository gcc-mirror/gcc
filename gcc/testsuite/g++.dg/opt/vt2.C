// PR c++/34949
// { dg-options "-O3" }
// { dg-final { scan-assembler-not "mov\[^\n\]*_ZTV" { target i?86-*-* x86_64-*-* } } }

class Foo
{
public:
  virtual ~Foo();
};

Foo::~Foo()
{
}


class Bar : public Foo
{
public:
  virtual ~Bar();
};

Bar::~Bar()
{
}
