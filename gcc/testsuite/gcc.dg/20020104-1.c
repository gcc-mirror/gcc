/* Test whether statement with no effect warnings are not given for last
   statements inside of statement expression.  */
/* { dg-do compile } */
/* { dg-options "-O -Wall" } */

void bar (char *p, char *q);

int main()
{
  char foo [32], *p;

  ({
    void *s = (foo);
    __builtin_memset (s, '\0', sizeof (foo));
    s; /* { dg-warning "no effect" "statement with no effect warning" } */
    s; /* { dg-bogus "no effect" "bogus statement with no effect warning" } */
  });
  p = foo;
  p;   /* { dg-warning "no effect" "statement with no effect warning" } */
  bar (foo, p);
  return 0;
}
