// PR c++/77522
// { dg-do compile { target c++14 } }

template < class T = int > void f (T)
{
  auto g = [&a = f] () {};  // { dg-error "invalid initialization" }
}

int main ()
{
  f (0);
  return 0;
}
