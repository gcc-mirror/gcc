// PR c++/60994
// { dg-do compile }

struct s
{
  static int i;
};

template <typename T>
int s()
{
  return s::i;	// { dg-bogus "is not a class" }
}
