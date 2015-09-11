// PR c++/64701
// { dg-options "" }

enum { A };
void
foo ()
{
  int x = ({ do {} while (0); A; });
}
