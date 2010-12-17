/* { dg-do compile } */

namespace n1 {
  void modf ();
}

namespace n2 {
  void trunc ();
  void modf ();
}
    
void max ()
{
  using n1::modf;
  using n2::trunc;
  using n2::modf;
}
