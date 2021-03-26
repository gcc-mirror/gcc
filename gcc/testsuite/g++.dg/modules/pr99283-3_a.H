// PR 99283 part 2 ICE on definition with qualified-name
// { dg-additional-options -fmodule-header }
// { dg-module-cmi {} }

template<bool, typename _Tp>
struct enable_if;

template<typename _Tp>
struct enable_if<true, _Tp>
{ typedef _Tp type; };

template<typename _CharT>
class basic_string;

typedef basic_string<char> string;

template<typename _CharT>
class basic_string
{
private:
  template<typename _Tp>
  using _If_sv = typename enable_if<true, int>::type;

public:
  const _CharT *c_str() const noexcept;
};

inline void stoi(const string& __str)
{
  __str.c_str ();
}
