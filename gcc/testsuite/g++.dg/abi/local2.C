// PR c++/42010
// { dg-final { scan-assembler "ZZN1A1fEvE1s" } }

struct A {
  static int f()
  {
    static struct {
      int i;
    } s;
    return s.i;
  }
};

int main()
{
  return A::f();
}
