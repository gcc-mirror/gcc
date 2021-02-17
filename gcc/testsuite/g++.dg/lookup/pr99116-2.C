// PR 99116, we need to remove the namespace-scope meaning of
// IDENTIFIER_TYPE_VALUE.

namespace __gnu_cxx 
{
template<typename _CharT>
struct char_traits
{
  static void length();
};

template<typename _T>
void char_traits<_T>::length ()
{
}

}
       
struct char_traits;
