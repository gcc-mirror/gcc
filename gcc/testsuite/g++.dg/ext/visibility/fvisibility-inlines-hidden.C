/* Test that -fvisibility-inlines-hidden affects class members. */
/* { dg-do compile } */
/* { dg-require-visibility "" } */
/* { dg-options "-fvisibility-inlines-hidden" } */
/* { dg-final { scan-hidden "_ZN3Foo6methodEv" } } */

class Foo
{
public:
  void method() { }
};

int main(void)
{
  Foo f;
  f.method();
  return 0;
}
