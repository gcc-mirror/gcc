// { dg-do assemble  }
// { dg-options "" }
// Origin: Mark Mitchell <mark@codesourcery.com>

void f ()
{
  int i = ({ l: 3; });
}
