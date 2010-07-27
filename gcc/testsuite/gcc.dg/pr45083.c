/* PR tree-optimization/45083 */
/* { dg-do compile } */
/* { dg-options "-O2 -Wuninitialized" } */

struct S { char *a; unsigned b; unsigned c; };
extern int foo (const char *);
extern void bar (int, int);

static void
baz (void)
{
  struct S cs[1];	/* { dg-message "was declared here" } */
  switch (cs->b)	/* { dg-warning "cs\[^\n\r\]*\\.b\[^\n\r\]*is used uninitialized" } */
    {
    case 101:
      if (foo (cs->a))	/* { dg-warning "cs\[^\n\r\]*\\.a\[^\n\r\]*may be used uninitialized" } */
	bar (cs->c, cs->b);	/* { dg-warning "cs\[^\n\r\]*\\.c\[^\n\r\]*may be used uninitialized" } */
    }
}

void
test (void)
{
  baz ();
}
