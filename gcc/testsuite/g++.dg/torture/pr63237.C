// { dg-do compile }

class A {
    int Length;
public:
    A(const char *p1) { Length = __builtin_strlen(p1); }
};
class B {
public:
    void m_fn1(int, A);
};
class C {
public:
    B &m_fn2();
};
int a;
void RewriteMacrosInInput() {
    C b;
    B &c = b.m_fn2();
    c.m_fn1(0, &""[a]);
}
