/* { dg-do compile } */

static void s() __attribute__((target("avx")));
static void s() { }
void f() {
        s();
}
