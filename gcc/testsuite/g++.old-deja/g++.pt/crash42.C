// Build don't link:
// Origin: Walter Brisken <walterfb@puppsr14.princeton.edu>

template <class T> class list {};

class newtype
{
};

void crash()
{
  newtype* n;
  n->list.size (); // ERROR - invalid use of template
}
