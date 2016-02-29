// PR c++/69995
// { dg-do compile { target c++14 } }

#define assert(X) static_assert((X),#X)

#define CONSTEXPR constexpr

template <typename T, unsigned long Size>
struct array {
    T elems_[Size];

    constexpr T const& operator[](unsigned long n) const
    { return elems_[n]; }

    constexpr T& operator[](unsigned long n)
    { return elems_[n]; }
};

template <typename T>
CONSTEXPR void my_swap(T& a, T& b) {
    T tmp = a;
    a = b;
    b = tmp;
}

CONSTEXPR auto rotate2() {
    array<array<int, 2>, 2> result{};
    array<int, 2> a{{0, 1}};

    result[0] = a;
    my_swap(a[0], a[1]);
    result[1] = a;

    return result;
}

int main() {
    CONSTEXPR auto indices = rotate2();
    assert(indices[0][0] == 0);
    assert(indices[0][1] == 1);
    assert(indices[1][0] == 1);
    assert(indices[1][1] == 0);
}
