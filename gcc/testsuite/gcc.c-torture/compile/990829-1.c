struct x
{
  int a:16;
  int b:16;
  int c;
};

bar()
{
  struct x y;
  y.b = 1 < y.a;
  foo(&y);
}
