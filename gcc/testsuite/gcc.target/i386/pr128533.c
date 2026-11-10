/* PR target/128533 */
/* { dg-do compile } */
/* { dg-options "-O0" } */

/* A complex value occupies two x87 stack registers,
   which reg-stack doesn't support for asm operands.  */

void f (void)
{
  float _Complex z = 0;
  long double v;
  asm volatile ("" : "=t" (v) : "0" (z));	/* { dg-error "must occupy a single register" } */
}
