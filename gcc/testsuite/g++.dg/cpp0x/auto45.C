// PR c++/66421
// { dg-do compile { target c++11 } }

template<typename... T> struct tuple { };

template<typename... T> tuple<T...> make_tuple(T&&...) { return {}; }

template <typename... Params>
void foo(Params... params) {
    auto t = make_tuple((int)params...);
}

template <typename... Params>
void bar(Params... params) {
  tuple<decltype((int)params)...> t = make_tuple((int)params...);
}

int main() {
    foo(1,2,3);
    bar(1,2,3);
}
