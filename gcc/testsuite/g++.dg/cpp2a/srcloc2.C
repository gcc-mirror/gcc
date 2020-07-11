// { dg-do compile { target c++20 } }

namespace std {
  inline namespace _8 { }
  namespace _8 {
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
}

using namespace std;

consteval source_location
bar (const source_location x = source_location::current ())
{
  return x;
}

void
foo (const char **p, unsigned *q)
{
  constexpr source_location s = source_location::current ();
  constexpr source_location t = bar ();
  p[0] = s.file_name ();
  p[1] = s.function_name ();
  q[0] = s.line ();
  q[1] = s.column ();
  p[2] = t.file_name ();
  p[3] = t.function_name ();
  q[2] = t.line ();
  q[3] = t.column ();
  constexpr const char *r = s.file_name ();
}

source_location s3 = source_location::current ();

template <int N>
constexpr source_location
baz ()
{
  return source_location::current ();
}

#define A \
  source_location s[3] = { source_location::current (), \
			   source_location::current (), \
			   source_location::current () }

source_location *
boo ()
{
  static A;
  return &s[0];
}

constexpr source_location s1 = baz <0> ();
constexpr source_location s2 = baz <1> ();
const source_location *p1 = &s1;
const source_location *p2 = &s2;

static_assert (source_location::current ().line () == __LINE__);
static_assert (source_location::current ().column () == 42);

constexpr bool
quux ()
{
  const char *file1 = source_location::current ().file_name ();
  const char *file2 = __FILE__;
  const char *function1 = source_location::current ().function_name ();
  const char *function2 = __FUNCTION__;
  int line1 = source_location::current ().line ();
  int line2 = __LINE__ - 1;
  int column
    = source_location::current ().column ();
  int i = 0;
  for (; file1[i]; i++)
    if (file1[i] != file2[i])
      return false;
  if (file2[i])
    return false;
  for (i = 0; function1[i]; i++)
    if (function1[i] != function2[i])
      return false;
  if (function2[i])
    return false;
  if (line1 != line2)
    return false;
  if (column != 33)
    return false;
  return true;
}

static_assert (quux ());
