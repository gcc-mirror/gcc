// Build don't link:
// Origin: Mark Mitchell <mark@codesourcery.com>
// Special g++ Options: -fsyntax-only

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
