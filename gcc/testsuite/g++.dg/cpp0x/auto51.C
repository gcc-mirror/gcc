// PR c++/84798
// { dg-do compile { target c++11 } }

template<typename T>
struct S {
    static constexpr T value = 0;
};

constexpr auto x = S<void(*)(auto)>::value; // { dg-error "auto" }
