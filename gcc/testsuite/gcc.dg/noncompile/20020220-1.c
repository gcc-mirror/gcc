/* PR c++/5713
   Test that there are no ICEs after redeclaration error.  */

int foo (const char*, const char*);

void bar (void)
{
  const char *s = "bar";
  int i;			/* { dg-error "previously declared here" } */
  int size = 2;
  int i = foo (s, s + size);	/* { dg-error "redeclaration of" } */
}
