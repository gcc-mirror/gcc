// PR c++/68948

struct B { B (); B (int); };

struct Time : B { };

/* Here, A and B are unrelated types.  */

template <typename>
struct A
{
  void TestBody ()
  {
    B::B (); // { dg-error "cannot call constructor .B::B." }
    B::B::B (); // { dg-error "cannot call constructor .B::B." }
    B::B (0); // { dg-error "cannot call constructor .B::B." }
  }
};

/* Here, C is (indirectly) derived from B.  */

template <typename g>
struct C : Time
{
  void TestBody ()
  {
    B::B (); // { dg-error "cannot call constructor .B::B." }
    B::B::B (); // { dg-error "cannot call constructor .B::B." }
    B::B (0); // { dg-error "cannot call constructor .B::B." }
    Time::B (0);
  }
};

int
main (void)
{
  A<int> a;
  C<int> c;
  a.TestBody ();
  c.TestBody ();
}
