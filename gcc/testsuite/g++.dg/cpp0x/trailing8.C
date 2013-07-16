// PR c++/54359
// { dg-require-effective-target c++11 }

int& ref(int& x) { return x; }
const int& ref(const int& x) { return x; }

class A {
    int x;
    int f() const;
    auto test1() const -> decltype(this);
    auto test2() const -> decltype(ref(x));
    auto test3() const -> decltype(f());
};

auto A::test1() const -> decltype(this) {
    return this;
}

auto A::test2() const -> decltype(ref(x)) {
    return ref(x);
}

auto A::test3() const -> decltype(f()) {
    return f();
}
