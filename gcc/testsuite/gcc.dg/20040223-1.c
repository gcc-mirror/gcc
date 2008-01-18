/* GCC was not warning about taking the address of parameters or
   fields of struct parameters when returning them.  */
/* PR c/14156 */

/* { dg-do compile  } */


int * f( int a)
{
	return &a;/* { dg-warning "address" "" } */
}

int * g()
{
	int b = 0;
	return &b;/* { dg-warning "address" "" } */
}

struct ll
{
  int i;
};

int *h(struct ll c)
{
  return &c.i;/* { dg-warning "address" "" } */
}


struct ll d;

int *i()
{
  return &d.i;/* { dg-bogus "address" "" } */
}


int *j(struct ll *c)
{
  return &c->i; /* { dg-bogus "address" "" } */
}
