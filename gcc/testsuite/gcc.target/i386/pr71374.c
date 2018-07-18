/* { dg-do compile } */
/* { dg-options "-O2" } */
int a, b, c;
void
foo (void *x, void *y)
{
  __asm__ ("": "=&c" (a), "=&D" (b), "=&S" (c): "r" (y), "2" (y));
}
