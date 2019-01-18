/* PR c/88664.  */
/* { dg-do compile } */
/* { dg-options "-O" } */

struct data
{
  void *ptr;
} __attribute__((packed));

int *
fun1 (struct data *p)
{
  return (int *) p->ptr;
}

int *
fun2 (struct data *p, int *x)
{
  return x ? (*x = 1, (int *) p->ptr) : (int *) 0;
}
