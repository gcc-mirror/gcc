// PR c++/47488 - sorry, unimplemented: string literal in function
// template signature
// { dg-do compile }
// { dg-options "-Wall" }

#if __cplusplus >= 201103L

// C++ 11 test case from comment #0.
namespace comment_0 {

template <typename T>
int f (const T&, const char *);

template <typename T>
decltype (f (T (), "")) g (const T &);

void h ()
{
  g (0);
}

}   // comment_0

#endif

// C++ 98 test case from comment #1.
namespace comment_1 {

template <typename T>
int  f(const T&, const char *);

template<int> struct N { };

template <typename T>
N<sizeof (f (T (), ""))> g (const T&);

void h ()
{
  g (0);
}

}   // comment_1

// C++ 98 test case from comment #2.
namespace comment_2 {

template <typename T>
int f (const char *);

template<int> struct N { };

template <typename T>
N<sizeof (f<T>(""))> g (const T &);

void h ()
{
  g (0);
}

}   // comment_2


#if __cplusplus >= 201103L

// C++ 11 test case from comment #5.
namespace comment_5 {

template <typename T> constexpr T f(const T* p) { return p[0]; }
template <int> struct N { };
template <typename T> void g (T, N<f((const T*)"1")>) { }
template <typename T> void g (T, N<f((const T*)"2")>) { }

void h ()
{
  g ('1', N<'1'>());
  g ('2', N<'2'>());
}

}

#endif
