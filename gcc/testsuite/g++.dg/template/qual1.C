// { dg-do compile }

template<class T>
class Link_array
{
public:
  void sort (int (*compare) (T *const&,T *const&));
};

int shift_compare (int *const &, int *const &) {}

template<class T> void
Link_array<T>::sort (int (*compare) (T *const&,T *const&)) 
{
}

void f ()
{
  Link_array<int> clashes;
  clashes.sort (shift_compare);
}
