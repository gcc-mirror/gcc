/* { dg-do compile } */
/* { dg-options "-std=c99 -Wpedantic" } */

unsigned int
foo (void)
{
  return sizeof ((int[]) {}); /* { dg-warning "ISO C forbids empty initializer braces" } */
}
