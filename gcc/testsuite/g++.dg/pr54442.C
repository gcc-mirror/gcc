/* { dg-do compile } */

struct S
{
  void s (int) const throw ();
  void s (int) throw ();
};

typedef int index_t;

void (S::*f) (index_t)       = &S::s;
void (S::*g) (index_t) const = &S::s;
