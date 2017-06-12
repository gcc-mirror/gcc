// PR c++/39219

enum __attribute__ ((deprecated)) E { e };
struct S { enum __attribute__ ((deprecated)) F { f = e }; };

int main () {
    E x;	// { dg-warning "'E' is deprecated" }
    x = e;

    S::F y;	// { dg-warning "'F' is deprecated" }
    y = S::f;

    return x + y;
}
