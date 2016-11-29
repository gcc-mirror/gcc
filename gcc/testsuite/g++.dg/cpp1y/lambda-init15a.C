// PR c++/71837
// { dg-do compile { target c++14 } }

template < typename ... Ts > void f (Ts ... args)
{
  [ts (args ...)] { return ts; } (); // { dg-error "" }
}

int main ()
{ 
  f ();				// { dg-message "required" }
  f (1,2);			// { dg-message "required" }
  return 0;
}
