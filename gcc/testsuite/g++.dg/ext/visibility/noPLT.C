/* Test that -fvisibility=hidden prevents PLT. */
/* { dg-do compile { target fpic } } */
/* { dg-require-visibility "" } */
/* { dg-options "-fPIC -fvisibility=hidden" } */
/* { dg-final { scan-assembler-not "methodEv@PLT" } } */

class Foo
{
public:
  void method();
};

void Foo::method() { }

int main(void)
{
  Foo f;
  f.method();
  return 0;
}
