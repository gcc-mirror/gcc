// { dg-do run { target c++14 } }

#include <cassert>

constexpr int n = 10;

struct A {
    constexpr operator const int*() const {
        return data;
    }
  
    constexpr operator int*() {
        return data;
    }
  
private:
    int data[n];
};

constexpr A f() {
    A a{};
    for (int i = 0; i < n; i++) {
        a[i] = i;
    }
    return a;
}

A a = f();

int main()
{
    for (int i = 0; i < n; i++) {
        assert (a[i] == i);
    }
}
