/* Gcc 3.3.1 deprecates memory inputs of non-lvalues.  */
/* { dg-do compile } */

void test(void)
{
  register int r;
  register int r2;
  int i;
  static int m;
  int *p;

  __asm__ ("" : : "m"(r));	/* { dg-warning "address of register" } */
  __asm__ ("" : : "m"(i));
  __asm__ ("" : : "m"(m));
  __asm__ ("" : : "m"(0));	/* { dg-warning "input without lvalue" } */
  __asm__ ("" : : "m"(i+1));	/* { dg-warning "input without lvalue" } */
  __asm__ ("" : : "m"(*p++));

  __asm__ ("" : : "g"(r));
  __asm__ ("" : : "g"(i));
  __asm__ ("" : : "g"(m));
  __asm__ ("" : : "g"(0));
  __asm__ ("" : : "g"(i+1));

  __asm__ ("" : "=m"(r2));	/* { dg-warning "address of register" } */
  __asm__ ("" : "=m"(i));
  __asm__ ("" : "=m"(m));
}
