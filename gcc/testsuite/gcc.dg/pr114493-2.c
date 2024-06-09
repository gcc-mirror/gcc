/* PR c/114493 */
/* { dg-do compile { target lto } } */
/* { dg-options "-O2 -flto -std=c23" } */

void foo (void);
struct S;
struct S bar (struct S **);
struct S qux (const struct S **);

void
corge (void)
{
  struct S { int s; } s;
  s.s = 0;
}

struct __attribute__((__may_alias__)) S {
  int s;
};

struct S
baz (void)
{
  foo ();
  return (struct S) {};
}
