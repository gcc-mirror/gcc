/* Functional tests for the "target" attribute and pragma.  */

/* { dg-do compile } */
/* { dg-require-effective-target target_attribute } */
/* { dg-options "-mno-mvcle -march=z13 -O3" } */

#pragma GCC target("no-mvcle")
void p0(char *b)
{
  __builtin_memset (b, 0, 400);
}
#pragma GCC reset_options

__attribute__ ((target("no-mvcle")))
void a0(char *b)
{
  __builtin_memset (b, 0, 400);
}

void d(char *b)
{
  __builtin_memset (b, 0, 400);
}

/* { dg-final { scan-assembler-not "\tmvcle\t" } } */
