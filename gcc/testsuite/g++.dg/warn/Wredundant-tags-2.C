// PR c++/93138
// { dg-do compile }
// { dg-options "-Wredundant-tags" }

struct Foo
{
  enum Kind { a };
private:
  Kind Kind;
};
enum Foo::Kind foo ();		// { dg-bogus "is private within this context|redundant" }
struct Bar
{
  struct Kind { int a; };
private:
  Kind Kind;
};
struct Bar::Kind bar ();	// { dg-bogus "is private within this context|redundant" }
