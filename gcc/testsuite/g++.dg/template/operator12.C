// PR c++/59707

struct T {
    template<class D> operator D*() const;
};

void f(T x) {
    x < x;			// { dg-error "no match" }
}
