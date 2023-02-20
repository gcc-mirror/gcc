// PR c++/106675
// { dg-do compile { target c++11 } }

struct foo {
    int n_;
    foo(int n) : n_(n) {}
};

struct bar {
    int n_;

    operator foo() const {
        return foo(n_);
    }
    operator foo &() { return *reinterpret_cast<foo *>(n_); }
    operator foo const &() = delete;

    void crashgcc() {
        foo tmp(*this); // { dg-error "ambiguous" }
    }
};
