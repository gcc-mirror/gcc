// PR c++/71822
// { dg-do compile }

int bar (int);

template <typename T>
struct A
{
  explicit A (int x = bar (sizeof (T)));
};

struct B
{
  A <int> b[2];
};

void
baz ()
{
  B b;
}
