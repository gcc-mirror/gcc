// P3769R1 - Clarification of placement new deallocation
// { dg-do run { target c++11 } }

namespace std
{
  using size_t = decltype (sizeof 0);
}

struct A {};
int v;
A a;
template <int I>
struct B {};
B <0> b;
template <int I>
struct C {};
C <0> c;
template <int I>
struct D {};
D <0> d;
struct T { T () = default; T (int) { throw 1; } };
union U { unsigned char c[sizeof (T)]; T t; } u;
void *foo ();
struct V {
  V () = default;
  V (int) { throw 2; }
  void *operator new (std::size_t, A &)
  {
    if (v != 5)
      __builtin_abort ();
    ++v;
    return foo ();
  }
  template <int = 0>
  void operator delete (void *, A &)
  {
    if (v != 6)
      __builtin_abort ();
    v += 2;
  }
  void *operator new (std::size_t, B <0> &)
  {
    if (v != 8)
      __builtin_abort ();
    v += 4;
    return foo ();
  }
  template <int I>
  void operator delete (void *, B <I> &)
  {
    __builtin_abort ();
  }
  void operator delete (void *, B <0> &)
  {
    __builtin_abort ();
  }
  void *operator new (std::size_t, C <0> &)
  {
    if (v != 12)
      __builtin_abort ();
    v += 8;
    return foo ();
  }
#if __cpp_concepts >= 202002L
  template <int I>
  requires (I > 0)
  void operator delete (void *, C <I> &)
  {
    __builtin_abort ();
  }
#endif
  void operator delete (void *, C <0> &)
  {
    if (v != 20)
      __builtin_abort ();
    v += 16;
  }
};
union W { unsigned char c[sizeof (V)]; V t; } w;

void *
foo ()
{
  return &w.c[0];
}

void *
operator new (std::size_t, A &)		// #1
{
  if (v != 0)
    __builtin_abort ();
  ++v;
  return &u.c[0];
}

template <int = 0>
void
operator delete (void *, A &)		// #2
{
  if (v != 1)
    __builtin_abort ();
  v += 2;
}

void *
operator new (std::size_t, B <0> &)	// #3
{
  if (v != 3)
    __builtin_abort ();
  v += 4;
  return &u.c[0];
}

template <int I>
void
operator delete (void *, B <I> &)	// #4
{
  __builtin_abort ();
}

void
operator delete (void *, B <0> &)	// #5
{
  __builtin_abort ();
}

void *
operator new (std::size_t, C <0> &)	// #6
{
  if (v != 7)
    __builtin_abort ();
  v += 8;
  return &u.c[0];
}

#if __cpp_concepts >= 202002L
template <int I>
requires (I > 0)
void
operator delete (void *, C <I> &)	// #7
{
  __builtin_abort ();
}
#endif

void
operator delete (void *, C <0> &)	// #8
{
  if (v != 15)
    __builtin_abort ();
  v += 16;
}

void *
operator new (std::size_t, double, int, float)
{
  if (v != 36)
    __builtin_abort ();
  v += 32;
  return &u.c[0];
}

void
operator delete (void *, double, int, float, int = 0, double = 0.0, float = 0.0)
{
  __builtin_abort ();
}

void *
operator new (std::size_t, double, int, long)
{
  if (v != 68)
    __builtin_abort ();
  v += 64;
  return &u.c[0];
}

void
operator delete (void *, double, int, long, int = 0, double = 0.0, float = 0.0)
{
  __builtin_abort ();
}

void
operator delete (void *, double, int, long)
{
  if (v != 132)
    __builtin_abort ();
  v += 128;
}

void *
operator new (std::size_t, unsigned, B <0> &, B <0> &)
{
  if (v != 9)
    __builtin_abort ();
  ++v;
  return &u.c[0];
}

template <typename T, typename U>
void operator delete (void *, unsigned, T &, U &)
{
  __builtin_abort ();
}

template <typename T>
void operator delete (void *, unsigned, T &, T &)
{
  __builtin_abort ();
}

template <int I>
void operator delete (void *, unsigned, B <I> &, B <I> &)
{
  __builtin_abort ();
}

void *
operator new (std::size_t, unsigned long, B <0> &, B <0> &)
{
  if (v != 10)
    __builtin_abort ();
  v += 2;
  return &u.c[0];
}

template <typename T, typename U>
void operator delete (void *, unsigned long, T &, U &)
{
  __builtin_abort ();
}

template <typename T>
void operator delete (void *, unsigned long, T &, T &)
{
  __builtin_abort ();
}

void *
operator new (std::size_t, unsigned long long, B <0> &, B <0> &)
{
  if (v != 12)
    __builtin_abort ();
  v += 4;
  return &u.c[0];
}

template <typename T, typename U>
void operator delete (void *, unsigned long long, T &, U &)
{
  if (v != 16)
    __builtin_abort ();
  v += 8;
}

void *
operator new (std::size_t, long long, B <0> &, B <0> &)
{
  if (v != 24)
    __builtin_abort ();
  v += 16;
  return &u.c[0];
}

template <typename T, typename U>
void operator delete (void *, long long, T &, U &) = delete;

template <typename T>
void operator delete (void *, long long, T &, T &) = delete;

template <int I>
void operator delete (void *, long long, B <I> &, B <I> &) = delete;

void *
operator new (std::size_t, D <0> &)
{
  if (v != 40)
    __builtin_abort ();
  v += 32;
  return &u.c[0];
}

template <int I>
void operator delete (void *, D <I> &)
{
  if (v != 72)
    __builtin_abort ();
  v += 64;
}

int
main ()
{
  try
    {
      T *p = new (a) T (1); // OK, uses #1 and #2.
      throw 1L;
    }
  catch (int)
    {
      if (v != 3)
	__builtin_abort ();
    }
  try
    {
      T *p = new (b) T (1); // OK, uses #3.  No deallocation function is selected (two candidates remain).
      throw 2L;
    }
  catch (int)
    {
      if (v != 7)
	__builtin_abort ();
    }
  try
    {
      T *p = new (c) T (1); // OK, uses #7 and #9.
      throw 3L;
    }
  catch (int)
    {
      if (v != 31)
	__builtin_abort ();
    }
  v = 5;
  try
    {
      V *p = new (a) V (1);
      throw 4L;
    }
  catch (int)
    {
      if (v != 8)
	__builtin_abort ();
    }
  try
    {
      V *p = new (b) V (1);
      throw 5L;
    }
  catch (int)
    {
      if (v != 12)
	__builtin_abort ();
    }
  try
    {
      V *p = new (c) V (1);
      throw 6L;
    }
  catch (int)
    {
      if (v != 36)
	__builtin_abort ();
    }
  try
    {
      T *p = new (0.0, 0, 0.0f) T (1);
      throw 7L;
    }
  catch (int)
    {
      if (v != 68)
	__builtin_abort ();
    }
  try
    {
      T *p = new (0.0, 0, 0L) T (1);
      throw 8L;
    }
  catch (int)
    {
      if (v != 260)
	__builtin_abort ();
    }
  v = 9;
  try
    {
      T *p = new (0U, b, b) T (1);
      throw 9L;
    }
  catch (int)
    {
      if (v != 10)
	__builtin_abort ();
    }
  try
    {
      T *p = new (0UL, b, b) T (1);
      throw 10L;
    }
  catch (int)
    {
      if (v != 12)
	__builtin_abort ();
    }
  try
    {
      T *p = new (0ULL, b, b) T (1);
      throw 12L;
    }
  catch (int)
    {
      if (v != 24)
	__builtin_abort ();
    }
  try
    {
      T *p = new (0LL, b, b) T (1);
      throw 13L;
    }
  catch (int)
    {
      if (v != 40)
	__builtin_abort ();
    }
  try
    {
      T *p = new (d) T (1);
      throw 14L;
    }
  catch (int)
    {
      if (v != 136)
	__builtin_abort ();
    }
}
