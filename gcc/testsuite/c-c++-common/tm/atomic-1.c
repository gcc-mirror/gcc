/* { dg-do compile } */

int g;
void f(void)
{
  __transaction_atomic {	/* { dg-error "without transactional memory" } */
    g++;
  }
}
