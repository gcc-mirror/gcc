/* { dg-do run } */

extern int foo(void) __attribute__((alias("bar_b")));

int bar_b(void) {
    return 1;
}
