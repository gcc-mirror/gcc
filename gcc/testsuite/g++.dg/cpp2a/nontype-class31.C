// PR c++/88092
// { dg-do compile { target c++20 } }

template<typename T>
struct S {
 constexpr S(...) { }
};

template <typename T> S(T) -> S<T>;

template <S s> struct foo { };

template <S s>
void fn ()
{
  auto t = s;
  foo<t> f1;
  foo<s> f2;
}
