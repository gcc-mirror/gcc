// PR middle-end/65409
// Reported by Ignacy Gawedzki <bugs@qult.net>

struct Foo
{
  Foo() {}
  int  a;
  int  b;
  char c;
};

Foo copy_foo(Foo);

struct Bar : Foo
{
  Bar(Foo t) : Foo(copy_foo(t)) {}
};

Bar a = Foo();
