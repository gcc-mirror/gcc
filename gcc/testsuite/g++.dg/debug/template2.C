// PR c++/57545

template<typename T, long unsigned int N>
struct array {
    T data[N];
};

template<typename T>
struct derived {
    typedef long unsigned int size_type;
    static const size_type n = 42;

    array<int, n> a;
};
