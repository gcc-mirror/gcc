// Build don't link:
// Origin: Mark Mitchell <mark@codesourcery.com>
// Special g++ Options: -Wunused

struct S
{
  S ();
  ~S ();
};

void f ()
{
  {
    S s1;
    int j; // WARNING - unused
    
  t:       // WARNING - unused
    S s2;
  }
}
