// Author: Dodji Seketeli <dodji@redhat.com>
// Origin: PR c++/26693
// { dg-do compile }


class Alpha
{
  typedef int X; // { dg-message "private" }
};

template<int>
class Beta
{
    typedef int Y; // { dg-message "private" }
};

template <int>
int
bar ()
{
  Beta<0>::Y i = 0;		// { dg-error "within this context" }
  return Alpha::X ();		// { dg-error "within this context" }
}

int i = bar<0> ();
