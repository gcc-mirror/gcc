typedef int v2si __attribute__ ((vector_size (8)));
typedef unsigned di __attribute__ ((mode(DI)));
void foo(unsigned long);
void bar() {
    v2si x = { 1, 2 };
    foo((di) x);
}
