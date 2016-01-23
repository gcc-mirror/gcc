// PR c++/69022 - attribute vector_size ignored with dependent bytes
// { dg-do compile }

template <int N>
struct A { static const int X = N; };

#if __cplusplus >= 201202L
#  define ASSERT(e) static_assert (e, #e)
#else
#  define ASSERT(e)   \
  do { struct S { bool: !!(e); } asrt; (void)&asrt; } while (0)
#endif

template <class T, int N>
struct B: A<N>
{
#if __cplusplus >= 201202L
  using A<N>::X;
#  define VecSize X
#else
#  define VecSize A<N>::X
#endif

    static void foo ()
    {
        char a __attribute__ ((vector_size (N)));
        ASSERT (sizeof a == N);

        T b __attribute__ ((vector_size (N)));
        ASSERT (sizeof b == N);
    }

    static void bar ()
    {
        char c1 __attribute__ ((vector_size (VecSize)));
        ASSERT (sizeof c1 == VecSize);

        char c2 __attribute__ ((vector_size (A<N>::X)));
        ASSERT (sizeof c2 == A<N>::X);

        T d1 __attribute__ ((vector_size (VecSize)));
        ASSERT (sizeof d1 == VecSize);

        T d2 __attribute__ ((vector_size (A<N>::X)));
        ASSERT (sizeof d2 == A<N>::X);
    }
};

void bar ()
{
    B<int, 16>::foo ();
    B<int, 16>::bar ();
}
