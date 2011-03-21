/* { dg-do compile } */
/* { dg-options "-O" } */

void
foo (const void *xxxxx, void *yyyyy, long y)
{
  asm volatile ("" :: "c" ((xxxxx)), "d" ((yyyyy)), "S" (y));
}
