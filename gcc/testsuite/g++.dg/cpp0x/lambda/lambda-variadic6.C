// PR c++/84160
// { dg-do compile { target c++11 } }

template < typename ... T > void f (T ... a) 
{
  [a ...] { [a ...] {}; };
}

void g ()
{
  f < int > (0);
}
