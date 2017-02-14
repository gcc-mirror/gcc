// PR c++/71837
// { dg-do compile { target c++14 } }

template < typename ... Ts > void f (Ts ... args)
{
  [ts (args ...)] { return ts; } ();
}

int main ()
{ 
  f (0);
  return 0;
}
