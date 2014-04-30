// PR c++/60980
// { dg-do compile { target c++11 } }

struct x0
{
  x0 () = default;
};
struct x1
{
  x0 x2[2];
  void x3 ()
  {
    x1 ();
  }
};
