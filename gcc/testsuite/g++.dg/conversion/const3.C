// PR c++/18177

void foo()
{
  X; // { dg-error "" }
  const_cast<int&>(X);
}
