// PR c++/78173
// { dg-do compile { target c++20 } }

template <class T>
concept CanDifference = requires(T x, T y) {
    x - y;
};

static_assert(!CanDifference<void*>);
