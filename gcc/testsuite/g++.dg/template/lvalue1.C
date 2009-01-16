// PR c++/38877

template<class _T1, class _T2>
struct pair
{
  typedef _T1 first_type;
  typedef _T2 second_type;
  _T1 first;
  _T2 second;
  pair () : first(), second() { }
  pair(const _T1& __a, const _T2& __b)
    : first(__a), second(__b) { }
};

template<class _T1, class _T2>
inline pair<_T1, _T2>
make_pair(_T1 __x, _T2 __y)
{
    return pair<_T1, _T2>(__x, __y);
}

template <int dim> class bar;

template <int dim>
pair<bar<dim> *, unsigned int>
foo (unsigned int position)
{  
      const pair<int,unsigned int> tmp;
      return make_pair (new bar<dim>(tmp.first),
                             position);
 }
