namespace A{
  int i;
  int f();
}

int A::f()
{
  return i;
}

main()
{
  return A::f();
}
