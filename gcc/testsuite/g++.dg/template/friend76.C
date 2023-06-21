// PR c++/69836
// { dg-do compile { target c++11 } }

template<int N = 25> struct number : public number<N - 1> {
    static constexpr int value = N;
    static constexpr number<N-1> prev() { return {}; }
};
template<> struct number<0> { static constexpr int value = 0; };

template<int N> struct S { enum { value = N }; };

template<int X> constexpr S<X+1> increment(S<X>) { return {}; }

#define RETURN(R) -> decltype(R) { return R; }

#define INIT(TYPE) \
        using W_ThisType = TYPE;  \
        friend constexpr S<0> state(number<0>, W_ThisType**) { return {}; }

#define STUFF \
    friend constexpr auto state(number<decltype(state(number<>{}, static_cast<W_ThisType**>(nullptr)))::value+1> w_counter, \
                                W_ThisType **w_this) \
        RETURN(increment(state(w_counter.prev(), w_this)))


template <typename T> struct TemplateObject   {
    INIT(TemplateObject)
    STUFF
    STUFF
};

int main() {
  TemplateObject<int> t;
    constexpr auto s = state(number<>{}, static_cast<TemplateObject<int>**>(nullptr)) ;
    static_assert(s.value == 2, "");
}
