// PR c++/56447
// { dg-do compile { target c++11 } }

template <class T>
void f()
{
  int i;
  // This lambda should not have a conversion op, since it captures i
  int (*p)() = [=]{ return i; }; // { dg-error "cannot convert" }
}

int main()
{
  f<int>();
}
