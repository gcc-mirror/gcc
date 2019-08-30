// PR c++/86669
// { dg-do compile }

struct S { S (); };
struct T : public S {};

S::S ()
{
  int *p = { (int *) &p };
}
