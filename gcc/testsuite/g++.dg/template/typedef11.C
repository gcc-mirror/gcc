// Author: Dodji Seketeli <dodji@redhat.com>
// Origin: PR c++/26693
// { dg-do compile }


class Alpha
{
  typedef int X; // { dg-error "'typedef int Alpha::X' is private" }
};

template<int>
class Beta
{
    typedef int Y; // { dg-error "'typedef int Beta<0>::Y' is private" }
};

template <int>
int
bar ()
{
  Beta<0>::Y i = 0;		// { dg-error "within this context" }
  return Alpha::X ();		// { dg-error "within this context" }
}

int i = bar<0> ();
