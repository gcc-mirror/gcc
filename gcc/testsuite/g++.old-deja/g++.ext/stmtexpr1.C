// Build don't link:
// Origin: Mark Mitchell <mark@codesourcery.com>
// Special g++ Options:

void f ()
{
  int i = ({ l: 3; });
}
