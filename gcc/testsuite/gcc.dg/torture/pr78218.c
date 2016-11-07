/* { dg-do run } */

struct 
{
  int v;
} a[2];

int b; 

void __attribute__((noinline,noclone))
check ()
{
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
