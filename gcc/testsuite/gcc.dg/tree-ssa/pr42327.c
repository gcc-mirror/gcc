/* { dg-do compile } */
/* { dg-options "-O1 -fcheck-data-deps" } */

void foo(char *str)
{
   while (*str != 0) *str++ = 0;
}
