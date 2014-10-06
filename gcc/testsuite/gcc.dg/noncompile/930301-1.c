struct a *q;
void
f()
{
  q++;	/* { dg-error "pointer to" } */
}
