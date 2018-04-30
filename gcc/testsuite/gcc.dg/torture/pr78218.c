/* { dg-do run } */

struct v
{
  int v;
};

struct v a[2];

struct v *gp;

void __attribute__((noinline,noclone))
check (struct v *p)
{
  gp = p;
  if (a[0].v != 1)
    __builtin_abort ();
}

int main ()
{
  a[1].v = 1;
  a[0] = a[1];
  a[1].v = 0;
  check (a);
  return 0;
}
