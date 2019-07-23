  __attribute__((used))
  struct a {int a;} *p,**ptr=&p,q;
void
set3()
{
  *ptr=&q;
}
