// PR c++/91548 - fix detecting modifying const objects for ARRAY_REF.
// { dg-do compile { target c++17 } }

using size_t = decltype(sizeof(0));

template <typename T, size_t N>
constexpr T& impl(T const (&array)[N], size_t index) {
    return const_cast<T&>(array[index]);
}

template <typename T, size_t N>
struct my_array {
    constexpr T& operator[](size_t i) { return impl(elems, i); }
    constexpr T const& operator[](size_t i) const { return elems[i]; }
    T elems[N];
};

bool f(int i) {
    static constexpr auto table = []() {
        my_array<bool, 256> arr = {};
        arr[2] = true;
        return arr;
    }();
    return table[i];
}
