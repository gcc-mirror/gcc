// PR c++/29054
// { dg-do compile }

struct A
{
  template <typename T, typename U> static void create (U) {}
};

struct B
{
  friend void A::create <B, const char *> (const char *);
};

int
main ()
{
  A::create<B>("test");
}
