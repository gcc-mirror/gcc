/* Test for non-lvalue arrays: test that qualifiers on non-lvalues
   containing arrays do not remain when those arrays decay to
   pointers.  PR 35235.  */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

int a;

void
f (void)
{
  const struct {
    int a[1];
  } s;
  int *p1 = s.a; /* { dg-error "qualifier" } */
  int *p2 = (a ? s : s).a;
  /* In this case, the qualifier is properly on the array element type
     not on the rvalue structure and so is not discarded.  */
  struct {
    const int a[1];
  } t;
  int *p3 = t.a; /* { dg-error "qualifier" } */
  int *p4 = (a ? t : t).a; /* { dg-error "qualifier" } */
  /* The issue could also lead to code being wrongly accepted.  */
  const struct {
    int a[1][1];
  } u;
  const int (*p5)[1] = u.a;
  const int (*p6)[1] = (a ? u : u).a; /* { dg-error "pointer" } */
}
