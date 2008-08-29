/* PR 18050 : bogus warning with -Wsequence-point */
/* { dg-do compile } */
/* { dg-options "-Wsequence-point" } */

struct x
{
  int i;
};
void bar(struct x*, int *);

void foo(struct x *y)
{
  bar(y++, &y->i); /* { dg-warning "operation on 'y' may be undefined" } */
}

void zz(int a, int *b)
{
  *b = a;
}

void baz(void) {
  int a = 5;
  zz(++a, &a);  /* { dg-bogus "operation on 'a' may be undefined" } */
}
