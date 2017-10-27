// PR c++/80739
// { dg-do compile { target c++14 } }

using size_t = decltype(sizeof(0));
template <class T> struct element {
    constexpr element() noexcept: x0(0), x1(0), x2(0), x3(0) {}
    T x0; int x1, x2, x3;
};
template <class T> struct container {
    constexpr container() noexcept: data() {data = element<T>();}
    element<T> data;
};
template <class T> constexpr bool test() {
    return (container<T>(), true);
}
int main() {
    constexpr bool tmp0 = test<int>();
    constexpr bool tmp1 = test<size_t>();
    return tmp0 && tmp1;
}
