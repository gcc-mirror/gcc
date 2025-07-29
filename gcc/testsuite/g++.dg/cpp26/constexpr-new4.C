// PR c++/121068
// { dg-do compile { target c++26 } }

constexpr void *operator new (__SIZE_TYPE__, void *p) { return p; }
constexpr void *operator new[] (__SIZE_TYPE__, void *p) { return p; }

consteval int
foo()
{
    using T = int;
    union { T arr[3]; };
    new(arr) T[3]; // makes arr active
    for (int i = 0; i < 3; ++i)
      arr[i].~T();

    new (arr + 2) T{10}; // A

    return 1;
};

constexpr int g = foo();
