
namespace std
{
typedef long unsigned int size_t;
}

namespace __gnu_cxx
{
template<typename _CharT>
struct char_traits
{
  typedef _CharT char_type;

  static constexpr std::size_t
    length(const char_type* __s);
};

template<typename _CharT>
constexpr std::size_t
  char_traits<_CharT>::
  length(const char_type* __p)
{
  std::size_t __i = 0;
  return __i;
}
}

namespace std
{
template<class _CharT>
struct char_traits;

template<>
struct char_traits<char>
{
  typedef char char_type;

  static constexpr size_t
    length(const char_type* __s)
  {
    return __gnu_cxx::char_traits<char_type>::length(__s);
  }
};
}
