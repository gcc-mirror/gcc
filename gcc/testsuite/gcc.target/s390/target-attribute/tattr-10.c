/* Functional tests for the "target" attribute and pragma.  */

/* { dg-do compile } */
/* { dg-require-effective-target target_attribute } */
/* { dg-options "-mno-mvcle -march=z13 -O3" } */

#pragma GCC target("mvcle")
void p1(char *b)
{
  __builtin_memset (b, 0, 400);
}
#pragma GCC reset_options

__attribute__ ((target("mvcle")))
void a1(char *b)
{
  __builtin_memset (b, 0, 400);
}

/* { dg-final { scan-assembler-times "\tmvcle\t" 2 } } */
