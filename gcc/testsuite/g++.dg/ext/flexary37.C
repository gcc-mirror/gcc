// PR c++/92427
// { dg-do compile { target c++11 } }
// { dg-options "" }

class C {
private:
    int a; int b;
public:
    C(int A, int B) : a(A), b(B) { }
    ~C() { }
};

struct y { // { dg-error "unknown array size in delete" }
    int a; C b[];
} y = { 1, { { 2, 3 } } };
