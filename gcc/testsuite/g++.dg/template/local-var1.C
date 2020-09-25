// PR c++/97186, related to c++/97171 but with a variable
// { dg-do compile { target c++11 } }

namespace
{
  template <typename WF>
  void
  ml ()
  {
    extern WF cr;

    static_assert (sizeof (cr) == 12, "");
  }

  void
  qc ()
  {
    ml<int[3]> ();
  }
}
