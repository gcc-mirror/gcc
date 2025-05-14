// PR c++/118249
// { dg-do compile { target c++11 } }

template <int I>
void f() { }

template <int N>
struct array {
    constexpr int size() const { return N; }
};

extern array<10>& outer;

struct C {
    array<10> inner;

    void g() {
        f<outer.size()>(); // OK
        f<inner.size()>(); // was error: use of 'this' in a constant expression
    }
};
