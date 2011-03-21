
class FooBaseBase0
{
public:
  virtual ~FooBaseBase0 () {}
};

class FooBaseBase1
{
public:
  virtual void Bar() {}
};


class FooBase: public FooBaseBase0, public FooBaseBase1
{
public:
  virtual void Bar() {}
};

class Foo2: public FooBase
{
public:
  ~Foo2 ();
  virtual void Bar();
};

class Foo3: public FooBase
{
public:
  ~Foo3 ();
  virtual void Bar();
};
