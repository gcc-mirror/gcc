//  PR c++/7910
// { dg-do compile { target i?86-*-cygwin* i?86-*-mingw* x86_64-*-mingw* } }
// { dg-options { -Wall -W } }

class __attribute__((dllimport)) Foo
{
 public:
  virtual void dummy_foo_func(void)
    {}
  void dummy_foo_fun2();
  virtual ~Foo();  //  avoid warning  
};

void Foo::dummy_foo_fun2()	//  { dg-warning "redeclared without dllimport" }
{
}

class Bar : public Foo
{
public:
  ~Bar();
  void dummy_bar_func();
};

Bar::~Bar()
{}

void Bar::dummy_bar_func()
{}

// { dg-final { scan-assembler-not "__imp\[_\]*__ZN3Foo14dummy_foo_fun" } }
