/* PR c/51628.  */
/* { dg-do compile } */
/* { dg-options "-O" } */

struct A {
  int i;
} __attribute__ ((packed));

struct A p = {1};
int *addr;

int i, j;

void
foo1 (void)
{
  addr = (i = -1, &p.i);
/* { dg-warning "may result in an unaligned pointer value" "" { target *-*-* } .-1 } */
}

void
foo2 (void)
{
  addr = (i = -1, j = -2, &p.i);
/* { dg-warning "may result in an unaligned pointer value" "" { target *-*-* } .-1 } */
}

void
foo3 (void)
{
  addr = (i = -1, (j = -2, &p.i));
/* { dg-warning "may result in an unaligned pointer value" "" { target *-*-* } .-1 } */
}
