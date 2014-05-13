// PR c++/59705
// { dg-do compile { target c++11 } }

struct HasIter {
   typedef int * const_iterator;
};

struct NoIter {
};

template <typename T>
constexpr bool foo(const T &, typename T::const_iterator *)
{
  return true;
}

template <typename T>
constexpr bool foo(const T &, ...)
{
  return false;
}

HasIter has_iter;
NoIter no_iter;

static_assert (!foo(no_iter, 0), "");
static_assert (foo(has_iter, 0), "");
static_assert (foo<HasIter>(has_iter, 0), "");
