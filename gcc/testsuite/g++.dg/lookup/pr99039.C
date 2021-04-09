// PR 99039, we need to remove the namespace-scope meaning of
// IDENTIFIER_TYPE_VALUE.

namespace std
{
typedef long unsigned int size_t;

template<typename _CharT>
struct char_traits
{
  typedef _CharT char_type;

  template<typename U>
  static int
    compare(const char_type* __s1, const char_type* __s2, std::size_t __n);
};

template<typename _CharT>
template<typename U>
int
char_traits<_CharT>::
compare(const char_type* __s1, const char_type* __s2, std::size_t __n)
{
  return 0;
}

}

struct CHAR_TRAITS;
namespace std
{
typedef long unsigned int size_t;

struct CHAR_TRAITS
{
  typedef char char_type;

  static int
    compare(const char_type* __s1, const char_type* __s2, std::size_t __n);
};

int
CHAR_TRAITS::
compare(const char_type* __s1, const char_type* __s2, std::size_t __n)
{
  return 0;
}

}

struct CHAR_TRAITS;
