// { dg-do run  }
namespace A{
  int i;
  int f();
}

int A::f()
{
  return i;
}

int main()
{
  return A::f();
}
