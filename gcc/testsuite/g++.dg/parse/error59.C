// PR c++/84493

void foo()
{
  (struct {}x){}; // { dg-error "" }
}
