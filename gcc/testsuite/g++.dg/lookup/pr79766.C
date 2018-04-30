// { dg-do compile { target c++11 } }
// PR 79766 qualified name to find inline namespace is ok

namespace Y 
{
  inline namespace X
  {
    void Q ();
  }
}

void Y::Q () // OK -> Y::X::Q
{
}

inline namespace Z
{
  void R ();
}

void ::R () // OK -> Z::R
{
}

void S ();

void ::S () // { dg-error "explicit qualification" }
{
}
