void something_f(float);

int foo(void)
{
  union
  {
    float f;
    double d;
  } u, *pu = &u;

  u.f = 1.0;
  something_f(u.f);
}
