struct X
{
  X () { }
  mutable int x;
};

main ()
{
  const X x;
  x.x = 0;
}
