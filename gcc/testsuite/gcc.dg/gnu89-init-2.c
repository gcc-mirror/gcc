/* Test whether __compound_literal.* objects are not emitted unless
   they are actually needed.  */
/* Origin: Jakub Jelinek <jakub@redhat.com> */
/* { dg-do compile } */
/* { dg-options "-std=gnu89 -O2" } */
/* { dg-final { scan-assembler-not "__compound_literal" } } */

struct A { int i; int j; int k[4]; };
struct B { };
struct C { int i; };
struct D { int i; struct C j; };

struct A a = (struct A) { .j = 6, .k[2] = 12 };
struct B b = (struct B) { };
int c[] = (int []) { [2] = 6, 7, 8 };
int d[] = (int [3]) { 1 };
int e[2] = (int []) { 1, 2 };
int f[2] = (int [2]) { 1 };
struct C g[3] = { [2] = (struct C) { 13 }, [1] = (const struct C) { 12 } };
struct D h = { .j = (struct C) { 15 }, .i = 14 };
struct D i[2] = { [1].j = (const struct C) { 17 },
		  [0] = { 0, (struct C) { 16 } } };
static const int *j = 1 ? (const int *) 0 : & (const int) { 26 };
int k = (int) sizeof ((int [6]) { 1, 2, 3, 4, 5, 6 }) + 4;
int l = (int) sizeof ((struct C) { 16 });
