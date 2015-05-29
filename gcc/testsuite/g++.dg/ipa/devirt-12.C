// { dg-do compile { target c++11 } }
// { dg-options "-O -fdump-ipa-inline" }

class Foo
{
public:
  void Bar() const
    {
      __builtin_puts ("Howdy!");
    }
};

int main()
{
  Foo x;
  auto y = &Foo::Bar;
  (x.*y)();
  return 0;
}

// { dg-final { scan-ipa-dump "Inlined 1 calls, eliminated 1 functions" "inline" } }
