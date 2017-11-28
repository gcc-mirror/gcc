// { dg-do compile }

// Origin: benko@sztaki.hu

// PR c++/10496: Incorrect pointer to member function diagnostics
// for constant member functions.

struct a
{
  int f() const;
};


int
a::f() const
{
  int (a::* b)() const = &f; // { dg-error "&a::f" }
  return 0;
}
