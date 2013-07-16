// PR c++/56611
// { dg-do compile { target c++11 } }

template<class T> struct remove_reference     { typedef T type; };
template<class T> struct remove_reference<T&> { typedef T type; };
template<class T> T declval() { return T(); }

int f(int, int){return 0;}
struct Func{};

template<class... Args> using result2
= decltype(f(declval<typename remove_reference<Args>::type>()...));

template<class Sig> struct R;
template<class This, class... Args> struct R< This(Args...) > 
{
  typedef result2<Args...> type;
};

typedef R< Func(int, int) >::type R_type;
