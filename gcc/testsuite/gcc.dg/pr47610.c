/* PR middle-end/47610 */
/* { dg-do compile } */
/* { dg-options "-O2 -fpic" { target fpic } } */
struct S { const char *s; };
const struct S s[] = { "s" };
extern void foo (void (*) (void));
static void bar (void) {}
void baz () { foo (bar); }
