/* PR c/114493 */
/* { dg-do compile { target lto } } */
/* { dg-options "-O2 -flto" } */

void foo (void);
struct S;
struct S bar (struct S **);
struct S qux (const struct S **);

struct __attribute__((__may_alias__)) S {
  int s;
};

struct S
baz (void)
{
  foo ();
  return (struct S) {};
}
