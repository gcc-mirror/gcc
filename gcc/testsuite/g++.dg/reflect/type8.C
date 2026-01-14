// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test splice-expressions in decltype.

template<class, class> struct same_type;
template<class T> struct same_type<T, T> {};

struct S {
  static const int &&mem();
  static int i;
  int j;
} s;

const int && foo();
decltype(foo()) x1 = 17;       // type is const int&&
same_type<decltype(x1), const int &&> s1;
decltype([: ^^x1 :]) x5 = 18;    // type is const int&&
same_type<decltype(x5), const int &&> s2;
decltype(([: ^^x1 :])) x6 = 19;  // type is const int&
same_type<decltype(x6), const int &> s3;
same_type<decltype(foo), const int&&()> s4a;
same_type<decltype([: ^^foo :]), const int&&()> s4b;
same_type<decltype(S::i), int> s5a;
same_type<decltype([: ^^S::i :]), int> s5b;
same_type<decltype((S::i)), int&> s6a;
same_type<decltype(([: ^^S::i :])), int&> s6b;
same_type<decltype(S::mem), const int&&()> s7a;
same_type<decltype([: ^^S::mem :]), const int&&()> s7b;
same_type<decltype((S::mem)), const int&& (&)()> s8a;
same_type<decltype(([: ^^S::mem :])), const int&& (&)()> s8b;
same_type<decltype(s.j), int> s9a;
same_type<decltype([: ^^s :].j), int> s9b;
same_type<decltype((s.j)), int&> s10a;
same_type<decltype(([: ^^s :].j)), int&> s10b;

namespace N {
  struct A { } a;
}

same_type<decltype(N::a), N::A> s11a;
same_type<decltype([: ^^N :]::a), N::A> s11b;
same_type<decltype([: ^^N::a :]), N::A> s11c;

struct B {
  int i : 31;
} b;

same_type<decltype(b.i), int> s12a;
same_type<decltype([: ^^b :].i), int> s12b;
same_type<decltype((b.i)), int&> s13a;
same_type<decltype(([: ^^b :].i)), int&> s13b;

template<typename T>
struct C {
  static constexpr T t{};
};

template<typename T>
void
g ()
{
  same_type<decltype(C<T>::t), const int>();
  same_type<decltype((C<T>::t)), const int&>();
  same_type<decltype([: ^^C<T>::t :]), const int>();
  same_type<decltype(([: ^^C<T>::t :])), const int&>();
  same_type<decltype([: ^^C<T> :]::t), const int>();
  same_type<decltype(([: ^^C<T> :]::t)), const int&>();
  same_type<decltype(template [: ^^C :]<T>::t), const int>();
  same_type<decltype((template [: ^^C :]<T>::t)), const int&>();
}

void
doit ()
{
  g<int> ();
}
