/* middle-end/122348 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

struct S {
    int a;
    int b[];
};
const struct S s = { 0, { 42 } };
void foo(struct S arg);
void f(void) {
    foo(s);
}
