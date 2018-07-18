// { dg-do compile { target c++11 } }

template<typename T, typename U> struct is_same {
  static const bool value = false;
};

template<typename T> struct is_same<T, T> {
  static const bool value = true;
};

template<typename...> struct Tuple {};
template<typename T1, typename T2> struct Pair {};

template<typename... Args1>
  struct zip {
    template<typename... Args2>
    struct with {
      typedef Tuple<Pair<Args1, Args2>...> type; // { dg-error "mismatched argument pack" }
    };
  };

static_assert 
  (is_same<zip<short, int>::with<unsigned short, unsigned>::type,
           Tuple<Pair<short, unsigned short>, Pair<int, unsigned> > >::value,
   "zip");

typedef zip<short>::with<unsigned short, unsigned>::type T2; // error: different number of arguments specified 
                                                             // for Args1 and Args2

template<typename... Args> void f(Args...);

template<typename... Args> void g(Args... args) 
{
   f(const_cast<const Args*>(&args)...); // okay: ``Args'' and ``args'' are expanded
   f(5 ...); // { dg-error "contains no parameter packs" }
   f(args); // { dg-error "5:parameter packs not expanded" }
   // { dg-message "args" "note" { target *-*-* } .-1 }
   f(h(args...) + args...); // okay: first ``args'' expanded within h, second ``args'' expanded within f.
}
