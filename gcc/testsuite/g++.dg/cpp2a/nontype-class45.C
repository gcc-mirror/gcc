// PR c++/99200
// { dg-do compile { target c++20 } }

template <int N>
struct A
{
  constexpr A (const char (&s)[N]) { for (int i = 0; i < N; i++) v[i] = s[i]; v[N] = 0; }
  char v[N + 1];
};

template <A s>
struct B
{
  constexpr operator const char *() { return s.v; }
};

template <typename T>
const char *
foo ()
{ 
  return B<__PRETTY_FUNCTION__>{};
}

template <typename T>
const char *
bar ()
{ 
  return B<__FUNCTION__>{};
}

auto a = foo <int> ();
auto b = bar <double> ();
