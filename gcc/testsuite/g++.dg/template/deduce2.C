template <typename T0> struct tuple {
    typedef tuple<int> tail;
};

template <> struct tuple<int> {
};

template <typename L>
struct length  {
  static const int i = length<typename tuple<L>::tail>::i;
};

template<>
struct length<tuple<int> > {
    static const int i = 1;
};

template <int> struct M {};

template <typename A>
M<length<tuple<A> >::i > foo (A*);

template <typename A>
M<length<tuple<A> >::i> foo (const A*);

const int i1 = 3;

void bar() {
  foo (&i1);
}
