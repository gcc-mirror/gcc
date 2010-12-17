/* PR middle-end/45234 */
/* { dg-do compile } */
/* { dg-options "-march=i586" { target ilp32 } } */

struct S { union { double b[4]; } a[18]; } s, a[5];
void foo (struct S);
struct S bar (struct S, struct S *, struct S);

void
foo (struct S arg)
{
}

void
baz (void)
{
 foo (bar (s, &a[1], a[2]));
}
