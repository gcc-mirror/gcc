// { dg-do assemble  }

template <class T> class List;
 
template <class T>
struct ListIterator
{
  ListIterator ();
  ListIterator (const ListIterator<T>& rhs);
};

template <class T>
struct List
{
  void length () const {
    for (ListIterator<T> li; li; ); // { dg-error "" } used where a `bool'
  }
};

void test(List<int>& vals)
{
  vals.length();
}
