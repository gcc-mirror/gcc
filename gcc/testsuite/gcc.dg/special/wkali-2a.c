/* { dg-do run } */

extern int foo(void) __attribute__((weak, alias("bar_a")));

int bar_a(void) {
    return 0;
}
