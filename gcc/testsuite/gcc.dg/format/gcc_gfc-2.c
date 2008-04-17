/* PR c/35436 */
/* { dg-do compile } */
/* { dg-options "-Wformat" } */

typedef void locus[1]; /* { dg-error "array of void" } */

void foo(const char*, ...)
  __attribute__((__format__(__gcc_gfc__, 1, 2))); /* { dg-error "locus" } */

void bar()
{
  foo("%L", 0); /* { dg-warning "format" }  */
}
