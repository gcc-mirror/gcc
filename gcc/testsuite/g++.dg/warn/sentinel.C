/* { dg-do compile } */
/* { dg-options "-Wall" } */
extern void ex (int i, ...) __attribute__ ((__sentinel__(0)));

void f()
{
  ex (1, 0);		/* { dg-warning "missing sentinel in function call" } */
  ex (1, 0L);		/* { dg-warning "missing sentinel in function call" } */
  ex (1, (void *)0);
  ex (1, __null);	/* { dg-bogus "sentinel" } */
}
