// PR c++/120635
// { dg-do compile { target c++11 } }

template <typename T, typename U>
struct [[clang::no_specializations]] A;
template <typename T, typename U>
struct A {};
template <>
struct A <int, int> { int a; };			// { dg-error "'struct A<int, int>' cannot be specialized" }
#if __cpp_variable_templates >= 201304
template <typename T>
struct B {
  template <typename U>
  static int b [[clang::no_specializations]];
};
template <>
template <>
int B<int>::b <long> = 43;			// { dg-error "'B<int>::b<long int>' cannot be specialized" "" { target c++14 } }
#endif
template <typename T>
[[clang::no_specializations]] int foo ();
template <typename T>
int foo () { return 42; }
template <>
int foo <long> () { return 43; }		// { dg-error "'int foo\\\(\\\) \\\[with T = long int\\\]' cannot be specialized" }
