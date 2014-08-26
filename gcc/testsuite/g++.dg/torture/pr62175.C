// { dg-do compile }
// { dg-additional-options "-ftrapv" }

struct B {
    B(int = 0);
};
int c;
int *d;
struct G {
    G();
    int numProcs_;
};
int fn1();
B fn2() {
    if (c)
      return 0;
    return B();
}

long &fn3(long &p1, long &p2) {
    if (p2 < p1)
      return p2;
    return p1;
}

void fn4(long p1) {
    long a = fn1();
    fn2();
    int b = fn3(p1, a);
    for (int i; i < b; ++i)
      d[0] = 0;
    for (; a < p1; ++a)
      d[a] = 0;
}

G::G() { fn4(numProcs_ + 1); }
