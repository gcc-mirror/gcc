// PR c++/70781
// { dg-do compile { target c++11 } }

template < typename T >  
void foo ()
{
  T ([=] (S) { [=] {}; }); 	// { dg-error "" }
}
