/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-sink1-details" } */

struct S {
    int* x;
    int* y;
};

void __attribute__((noreturn)) bar(const struct S* s);

void foo(int a, int b) {
    struct S s;
    s.x = &a;
    s.y = &b;
    if (a < b) {
        bar(&s);
    }
}

/* { dg-final { scan-tree-dump "Sinking.*s.y" "sink1" } } */
/* { dg-final { scan-tree-dump "Sinking.*s.x" "sink1" } } */
