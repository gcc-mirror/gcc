struct X
{
  X () { }
  mutable int x;
};

int main ()
{
  const X x;
  x.x = 0;
}
