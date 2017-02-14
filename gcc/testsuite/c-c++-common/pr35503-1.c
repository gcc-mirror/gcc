/* { dg-do compile } */
/* { dg-options "-Wrestrict" } */

int foo (char *__restrict buf, const char *__restrict fmt, ...);

void f(void)
{
  char buf[100] = "hello";
  foo (buf, "%s-%s", buf, "world"); /*  { dg-warning "passing argument 1 to restrict-qualified parameter aliases with argument 3" } */
}
