unsigned short foo (void)
{
  unsigned short u[1] = { 1 };
  u[0] = 0;
  u[1] = 1;
  u[2] = 2;
  return u[0] + u[1] + u[2];
}
