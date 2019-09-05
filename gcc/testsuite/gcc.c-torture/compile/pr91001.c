/* PR middle-end/91001 */
/* PR middle-end/91105 */
/* PR middle-end/91106 */

struct __attribute__((packed)) S { short b; char c; };
struct T { short b, c, d; };
struct __attribute__((packed)) R { int b; char c; };
union __attribute__((aligned(128), transparent_union)) U { struct S c; } u;
union __attribute__((aligned(32), transparent_union)) V { struct T c; } v;
union __attribute__((aligned(32), transparent_union)) W { struct R c; } w;
void foo (union U);
void bar (union V);
void baz (union W);

void
qux (void)
{
  foo (u);
}

void
quux (void)
{
  bar (v);
}

void
corge (void)
{
  baz (w);
}
