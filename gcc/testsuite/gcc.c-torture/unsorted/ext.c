struct foo
{
  unsigned b31 : 1;
  unsigned b30 : 1;
  unsigned b29 : 1;
  unsigned b28 : 1;
  unsigned rest : 28;
};
foo(a)
     struct foo a;
{
  return a.b30;
}
