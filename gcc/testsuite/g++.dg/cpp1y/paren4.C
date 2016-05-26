// PR c++/70822
// { dg-do compile { target c++14 } }

struct a
{
  static int b;
};

template <typename>
void
foo ()
{
  &(a::b);
}
