unsigned
rec (a, b)
     unsigned a;
     unsigned b;
{
  return a * rec (a - 1, b + 1);
}
