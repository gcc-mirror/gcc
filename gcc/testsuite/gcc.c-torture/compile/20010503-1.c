void f1 (double);
void f2 (int);

void
foo (int type, double xx)
{
  if (type)
    f1 (xx);
  else
    f2 (type);
}

void
bar (int type)
{
  foo (type, 1.0);
}
