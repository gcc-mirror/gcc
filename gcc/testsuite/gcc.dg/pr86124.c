/* { dg-do compile } */
/* { dg-options "-O -fipa-pta" } */

extern void a (void);

void b (void)
{
  void *c;
  c = a;
  *(char *)c = 1; /* { dg-warning "accessing data memory with program memory address.*" "" { target avr-*-* } } */
}
