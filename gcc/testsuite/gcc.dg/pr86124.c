/* { dg-do compile } */
/* { dg-options "-O -fipa-pta" } */
/* { dg-skip-if "acessing data memory with program memory address" { "avr-*-*" } } */

extern void a (void);

void b (void)
{
  void *c;
  c = a;
  *(char *)c = 1;
}
