// PR c++/52247
// { dg-do compile }

template <int N>
bool
bar ()
{
  __asm goto ("" : : : : lab);
  return true;
lab:
  return false;
}

bool
foo ()
{
  return bar<0> ();
}
