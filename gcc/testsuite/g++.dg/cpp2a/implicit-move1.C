// testcase from P1825R0
// { dg-do compile { target c++20 } }

struct base {
    base();
    base(base const &);
private:
    base(base &&);
};

struct derived : base {};

base f(base b) {
    throw b;        // { dg-error "" } base(base &&) is private
    derived d;
    return d;       // { dg-error "" } base(base &&) is private
}
