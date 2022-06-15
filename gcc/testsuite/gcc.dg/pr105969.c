/* { dg-do compile } */
/* { dg-options "-Wall" } */

struct A
{
  char a[0][0][0];
};
extern struct A b[][2];
void f (void)
{
  __builtin_sprintf (b[0][0].a[1][0], "%s", b[0][0].a[1][0]); /* { dg-warning "past the end" } */
  /* { dg-warning "overlaps destination" "" { target *-*-* } .-1 } */
}
