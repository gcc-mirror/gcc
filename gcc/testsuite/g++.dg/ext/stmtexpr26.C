// PR c++/116418
// { dg-do compile { target c++14 } }
// { dg-options "" }

void foo ();
template <typename>
void bar ()
{
  decltype(auto) v = ({ foo (); 3; });
}
