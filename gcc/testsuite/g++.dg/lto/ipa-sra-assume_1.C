struct S
{
  int x;
};

int
bar (struct S s)
{
  [[assume (s.x < 0)]];
  return s.x;
}
