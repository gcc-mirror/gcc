/* { dg-do compile } */
/* { dg-options "-O2" } */

class C
{
  int y, x;
  void i ();
  bool __attribute__((const)) xx () { return x; }
};

void C::i ()
{
  if (xx ())
    x = 1;
}
