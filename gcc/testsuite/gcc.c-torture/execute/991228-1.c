__extension__ union { double d; int i[2]; } u = { d: -0.25 };

int
signbit(double x)
{
  __extension__ union { double d; int i[2]; } u = { d: x };
  return u.i[0] < 0;
}
    
int main(void)
{
  if (2*sizeof(int) != sizeof(double) || u.i[0] >= 0)
    exit(0);

  if (!signbit(-0.25))
    abort();

  exit(0);
}
