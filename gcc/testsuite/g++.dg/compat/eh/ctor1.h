struct Foo
{
  ~Foo ();
};

struct Bar
{
  ~Bar () throw(int);
  Foo f;
};
