struct a *q;
f()
{
  q++;	/* { dg-error "pointer to" } */
}
