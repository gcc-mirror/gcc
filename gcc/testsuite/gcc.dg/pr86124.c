/* { dg-do compile } */
/* { dg-options "-O -fipa-pta" } */

extern void a (void);

void b (void)
{
  void *c;
  c = a;
  *(char *)c = 1;
}
