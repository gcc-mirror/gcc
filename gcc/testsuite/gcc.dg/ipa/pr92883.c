/* PR ipa/92883 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

int a, b, c, d;
unsigned e;
void baz (void *, int);
void grault (int, unsigned long);
int foo (unsigned g) { return a / g; }
void bar (void *g) { if (b) baz (g, 5); }
static void quux (int, unsigned long);
static void qux (unsigned long g) { if (g) { d = foo (-1); quux (e, (d & 2) + g); } }
static void quux (int g, unsigned long m) { (void) g; grault (c, m); bar (""); }
void corge () { qux (e); }
