// Build don't link:
// GROUPS passed templates membertemplates
template <int N>
struct IndexPlaceholder {};

template <int N1, int N2, int N3>
struct ArrayIndexMapping {};

template <class T_numtype, int N_rank>
struct Array
{
  template<int N0, int N1>
  ArrayIndexMapping<N_rank, N0, N1> 
  f(IndexPlaceholder<N0>, IndexPlaceholder<N1>);
};


template <class T_numtype>
void foo(T_numtype)
{
  Array<T_numtype, 1> t;
}
