/* PR c/88664.  */
/* { dg-do compile } */
/* { dg-options "-O" } */

struct data
{
  void *ptr;
} __attribute__((packed));

void **
fun1 (struct data *p)
{
  return &p->ptr;
/* { dg-warning "may result in an unaligned pointer value" "" { target *-*-* } .-1 } */
}

int *
fun2 (struct data *p, int *x)
{
  return p ? (*x = 1, (int *) &p->ptr) : (int *) 0;
/* { dg-warning "may result in an unaligned pointer value" "" { target *-*-* } .-1 } */
}
