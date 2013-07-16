// PR c++/57252
// { dg-require-effective-target c++11 }

struct foo {
  void bar() & {}
  void bar() && {}
};

int main()
{
  auto p = &foo::bar;		// { dg-error "" }
  (foo{}.*p)();
}
