// { dg-final { scan-assembler-not "_ZN1AD2Ev" } }

struct A { };

int main()
{
  A a;
  a.~A();
}
