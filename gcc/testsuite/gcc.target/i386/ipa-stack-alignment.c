/* { dg-do compile } */
/* { dg-options "-fno-ipa-stack-alignment -O" } */

typedef struct {
  long a;
  long b[];
} c;

c *d;
void e() { d->b[0] = 5; }
void f() { e(); }

/* { dg-final { scan-assembler "sub.*%.sp" } } */
