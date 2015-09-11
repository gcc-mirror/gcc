// { dg-do compile }

extern "C" double acos(double);
class A {
public:
    double mY, mZ;
    A(double, double);
    double m_fn1(A *);
    int *m_fn2();
};
double a;
A *b;
A::A(double, double) : mY(), mZ() {}

double A::m_fn1(A *) { return mY * mZ; }

inline int *A::m_fn2() {
    mZ = 0;
    double c = m_fn1(this);
    a = acos(c);
    double d = m_fn1(b);
    acos(d);
}

void passTime() {
    A e(0, 1);
    e.m_fn2();
}
