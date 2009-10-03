struct X;
struct Y
{
  struct X *p;
  int i;
};

void foo (struct Y *p)
{
  p->i = 1;
}
