// { dg-do compile { target i?86-*-cygwin* } }

class __attribute__((dllimport)) Foo
{
 public:
  virtual void dummy_foo_func(void)
    {}
};

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
