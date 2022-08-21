/* { dg-do compile } */
/* { dg-options "-O2 -mabi=call0 -foptimize-sibling-calls" } */

extern int foo(int);
extern void bar(int);

int test_0(int a) {
    return foo(a);
}

void test_1(int a) {
    bar(a);
}

int test_2(int (*a)(void)) {
    bar(0);
    return a();
}

_Complex double test_3(_Complex double a, _Complex double (*b)(_Complex double, double)) {
    bar(-1);
    return b(a, 3.141592653589795);
}

/* { dg-final { scan-assembler-not "ret" } } */
