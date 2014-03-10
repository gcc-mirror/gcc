// { dg-do compile { target c++11 } }
template<int... Indexes>
  struct _Index_tuple { };

template<int _Num, typename _Tuple = _Index_tuple<> >
struct _Build_index_tuple;

template<int _Num, int... _Indexes> 
struct _Build_index_tuple<_Num, _Index_tuple<_Indexes...> >
  : _Build_index_tuple<_Num - 1, 
                       _Index_tuple<_Indexes..., sizeof...(_Indexes)> >
{
};

template<int... _Indexes>
struct _Build_index_tuple<0, _Index_tuple<_Indexes...> >
{
  typedef _Index_tuple<_Indexes...> __type;
};
