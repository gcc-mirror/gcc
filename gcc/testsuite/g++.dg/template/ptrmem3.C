// Origin: Theo Papadopoulo <Theodore.Papadopoulo@sophia.inria.fr>

template <typename T,double (T::*fun)() const>
struct I {
};

struct R {
    R() { }
};

class H: public R {
public:
    H(): R() { }
    double& f() { return a; }
    double  f() const { return 1.0; }
    double a;
};

struct A {
    typedef I<H,&H::f> F;
    A() { }
};
