// PR c++/50732

template <typename T>
struct non_instantiable
{
  typedef typename T::THIS_TYPE_CANNOT_BE_INSTANTIATED type;
};

int check[__is_base_of(non_instantiable<int>, void) ? -1 : 1];
