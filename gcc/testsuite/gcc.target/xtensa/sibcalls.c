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

/* { dg-final { scan-assembler-not "ret" } } */
