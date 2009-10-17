struct X;
void foo (void *q)
{
  struct X **p = (struct X **)q;
  *p = (struct X *)0;
}
