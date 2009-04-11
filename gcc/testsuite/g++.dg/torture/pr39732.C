/* { dg-do compile } */
/* { dg-options "-fprofile-generate" } */

template<class _CharT>     struct char_traits;
template<typename _OI>
_OI __copy_move_a2(_OI __result);
template<typename _OI>     inline _OI
copy(_OI __result)
{
  return __copy_move_a2 (__result);
}
template<typename _CharT, typename _Traits>
class basic_ostream     { };
template<typename _Tp, typename _CharT = char, typename _Traits = char_traits<_CharT> >
class ostream_iterator      {
    typedef basic_ostream<_CharT, _Traits> ostream_type;
    ostream_type* _M_stream;
    const _CharT* _M_string;
public:
    ostream_iterator(ostream_type& __s, const _CharT* __c)
	: _M_stream(&__s), _M_string(__c) { }
    ostream_iterator(const ostream_iterator& __obj)
	: _M_stream(__obj._M_stream), _M_string(__obj._M_string) { }
};
int f(void)
{
  basic_ostream<char, char_traits<char> > os;
  copy(ostream_iterator<const int>(os, ","));
}
