// PR c++/95789
// { dg-do compile { target c++11 } }

struct B {
    int n;
};

template <typename T>
struct A {
    B& get() const { return f; } // { dg-error "binding reference" }

    B f;
};

int main() {
    A<int> a;
    a.f = {};

    a.get().n = 10;
    if (a.f.n != 0)
      __builtin_abort();
}
