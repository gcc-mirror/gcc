namespace {
  __attribute__((used))
  struct a {int a;} *p,**ptr=&p,q;
};
void
set2()
{
  *ptr=&q;
}
