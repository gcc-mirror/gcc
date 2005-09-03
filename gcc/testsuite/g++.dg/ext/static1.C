// PR c++/23699
// { dg-options "" }

template<typename _CharT > class basic_string;
typedef basic_string<char> string;
template<typename _CharT>
struct basic_string
{
  static const int npos = -1;
};
template<typename _CharT>
const int basic_string<_CharT>::npos;

extern template class basic_string<char>;
struct A
{
  static const long npos = string::npos;
};
