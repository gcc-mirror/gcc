/* { dg-do compile } */
/* { dg-options "-O2" } */
bool f();
struct S {
    S();
    ~S();
};
void g() {
    for (;;) {
        S s1, s2, s3, s4, s5, s6;
        if (f())
            continue;
        if (f())
            return;
    }
}
