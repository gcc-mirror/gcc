// { dg-do assemble  }
// { dg-options "-fsyntax-only" }
// Origin: Mark Mitchell <mark@codesourcery.com>

class AAA{
public:
  virtual void fff();
};

void AAA::fff() {}

AAA aaa;

main ()
{
  aaa.fff();
}
