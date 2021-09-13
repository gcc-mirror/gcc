/* PR lto/99849 */
/* { dg-lto-do link } */
/* { dg-require-effective-target fpic } */
/* { dg-require-effective-target shared } */
/* { dg-extra-ld-options { -shared } } */
/* { dg-lto-options { { -flto -flto-partition=1to1 -O2 -Wno-incompatible-pointer-types -Wno-discarded-qualifiers -fPIC } } } */

struct { struct A *a; } *b;
struct B { int *b; };
struct C { int *c; };
const struct D { struct C d; } d;
struct A { struct { struct C e; }; };
struct E { void *e; } e = { &( &(const struct D) { (void *) &d })->d };
struct C f = { &( &(const struct D) { (void *) &d })->d };
struct A g[] = { &e, &f };
void foo () { b->a = g; }
struct E h = { foo };
void bar ();
int baz () { bar (h); }
struct B i = { (int *) baz };
void qux ();
void corge () { qux (i); }
void *j __attribute__((__used__)) = corge;
