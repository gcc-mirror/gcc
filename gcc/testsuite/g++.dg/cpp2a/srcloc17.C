// { dg-do run { target c++20 } }

namespace std {
  struct source_location {
    struct __impl {
      const char *_M_file_name;
      const char *_M_function_name;
      unsigned int _M_line, _M_column;
    };
    const __impl *__ptr;
    constexpr source_location () : __ptr (nullptr) {}
    static consteval source_location
    current (const void *__p = __builtin_source_location ()) {
      source_location __ret;
      __ret.__ptr = static_cast <const __impl *> (__p);
      return __ret;
    }
    constexpr const char *file_name () const {
      return __ptr ? __ptr->_M_file_name : "";
    }
    constexpr const char *function_name () const {
      return __ptr ? __ptr->_M_function_name : "";
    }
    constexpr unsigned line () const {
      return __ptr ? __ptr->_M_line : 0;
    }
    constexpr unsigned column () const {
      return __ptr ? __ptr->_M_column : 0;
    }
  };
}

using namespace std;

template <int N>
constexpr source_location
foo (const source_location x = source_location::current ())
{
  return x;
}

template <int N>
struct S {
  const char *func;
  unsigned line = 0;
  source_location loc = source_location::current ();

  constexpr S (int l, source_location loc = source_location::current ())
  : func(__PRETTY_FUNCTION__), line(l), loc(loc)
  {}

  constexpr S (double)
  : func(__PRETTY_FUNCTION__), line(__LINE__)
  //                                        ^ column 45
  {}
};

constexpr bool
cmp (const char *p, const char *q)
{
  for (; *p && *q; p++, q++)
    if (*p != *q)
      return true;
  return *p || *q;
}

template <int N>
constexpr bool
bar ()
{
  int line = __LINE__;
  source_location a = foo<N> ();
  source_location b = source_location::current ();
  source_location c = foo<N> ();
  //                         ^ column 30
  //                                           ^ column 48
  const source_location *d[3] = { &a, &b, &c };
  const char *file1 = __FILE__;
  const char *function1 = __PRETTY_FUNCTION__;
  for (int j = 0; j < 3; j++)
    {
      int i= 0;
      if (cmp (d[j]->file_name (), file1))
	return false;
      if (cmp (d[j]->function_name (), function1))
	return false;
      if (d[j]->line () != line + j + 1)
	return false;
      if (d[j]->column () != (j == 1 ? 48 : 30))
	return false;
    }

  S<N> e (__LINE__);
  //   ^ column 8
  S<N> f = 1.0;
  if (cmp (e.loc.file_name (), file1))
    return false;
  if (cmp (f.loc.file_name (), file1))
    return false;
  if (cmp (e.loc.function_name (), function1))
    return false;
  if (cmp (f.loc.function_name (), f.func))
    return false;
  if (e.loc.line () != e.line)
    return false;
  if (f.loc.line () != f.line)
    return false;
  if (e.loc.column () != 8)
    return false;
  if (f.loc.column () != 45)
    return false;
  return true;
}

static_assert (bar<0> ());

int
main ()
{
  if (!bar<0> ())
    __builtin_abort ();
}
