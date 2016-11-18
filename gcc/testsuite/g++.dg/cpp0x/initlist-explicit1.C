// PR c++/67631
// { dg-do compile { target c++11 } }

struct X
{
  explicit operator unsigned ();
};
unsigned foo ()
{
  return unsigned{ X () };
}
