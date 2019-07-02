// { dg-additional-options -fmodules-ts }

export module foo;
// { dg-module-cmi foo }

template<typename _CharT >
struct basic_string
{
  template<typename = unsigned> basic_string();
};

inline basic_string<char>
to_string ()
{
  basic_string<char> __str;

  return __str;
}
