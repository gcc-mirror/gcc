extern void link_error ();

void foo(double x)
{
  if (x > __builtin_inf())
    link_error ();
}

int main ()
{
  foo (1.0);
  return 0;
}

