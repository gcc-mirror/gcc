// PR c++/17404
// { dg-do compile }
// { dg-options "" }

template <int> void foo ()
{
  __builtin_expect  (({0;}), 1);
}
 
template void foo<1> ();
