c_move_tst (char b)
{
  char a;

  a = b;
  b = 'b';
  foo (a);
  foo (b);
  foo (a);
  bar (a, b);
  b = a;
  if (b == 0)
    a++;
  return a + b;
}
