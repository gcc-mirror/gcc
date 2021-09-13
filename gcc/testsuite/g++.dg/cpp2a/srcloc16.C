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

struct S
{
  source_location a = source_location::current ();
  source_location b = source_location::current ();
  source_location c = source_location ();
  constexpr S () { c = source_location::current (); }
};

struct T
{
  int t;
  source_location u = source_location::current ();
  int v = __builtin_LINE ();
};

constexpr S s;
constexpr T t = { 1 };

constexpr bool
cmp (const char *p, const char *q)
{
  for (; *p && *q; p++, q++)
    if (*p != *q)
      return true;
  return *p || *q;
}

constexpr bool
foo ()
{
  T u = { 2 };
  source_location v = source_location::current ();
  if (cmp (s.a.file_name (), s.c.file_name ())
      || cmp (s.b.file_name (), s.c.file_name ())
      || cmp (t.u.file_name (), s.c.file_name ())
      || cmp (u.u.file_name (), s.c.file_name ())
      || cmp (v.file_name (), s.c.file_name ())
      || cmp (s.a.function_name (), s.c.function_name ())
      || cmp (s.b.function_name (), s.c.function_name ())
      || cmp (t.u.function_name (), "")
      || cmp (u.u.function_name (), v.function_name ())
      || s.a.line () != s.c.line ()
      || s.b.line () != s.c.line ()
      || t.u.line () != t.v
      || u.u.line () + 1 != v.line ()
      || s.a.column () != 18
      || s.b.column () != 18
      || s.c.column () != 49
      || t.u.column () != 21
      || u.u.column () != 13
      || v.column () != 48)
    return false;
  return true;
}

static_assert (foo ());

int
main ()
{
  if (!foo ())
    __builtin_abort ();
}
