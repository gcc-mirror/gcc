union u {
  unsigned char a;
  double b;
};

int a;

union u foo (void)
{
  union u b;

  if (a)
    b.a = 1;
  else
    b.a = 0;
  return b;
}
