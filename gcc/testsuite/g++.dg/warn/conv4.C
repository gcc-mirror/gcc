// { dg-do compile }

// This file should compile cleanly by default and not warn on the conversions.
int func1(int i)
{
  return i;
}

int main()
{
  float f;
  long l;
  unsigned long ul;

  f = 1.5f;

  l = f;
  ul = -1;
  func1(f);

  return 0;
}
