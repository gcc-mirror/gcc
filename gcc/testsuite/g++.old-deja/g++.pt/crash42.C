// { dg-do assemble  }
// Origin: Walter Brisken <walterfb@puppsr14.princeton.edu>

template <class T> class list {};

class newtype
{
};

void crash()
{
  newtype* n;
  n->list.size (); // { dg-error "" } invalid use of template
}
