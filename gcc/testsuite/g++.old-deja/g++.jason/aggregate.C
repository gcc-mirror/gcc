// { dg-do run  }
struct A { int i; };

int main()
{
  A a1 = { 42 };
  A a2 (a1);
  A a3 = { 137 };
  a1 = a3;

  if (a1.i == 137 && a2.i == 42 && a3.i == 137)
    return 0;
  return 1;
}
