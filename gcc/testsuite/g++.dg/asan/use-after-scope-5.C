// { dg-do run }

int *
__attribute__((optimize(("-O0"))))
fn1 (int *a)
{
  return a;
}

void
fn2 ()
{
  for (int i = 0; i < 10; i++)
    {
      int *a;
      (a) = fn1 (a);
    }
}

int main()
{
  fn2();
}
