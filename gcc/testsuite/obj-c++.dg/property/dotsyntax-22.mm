/* PR objc/47784.  This testcase used to crash the compiler.  */

typedef struct {
  float x;
} SomeType;

@interface MyClass

@property(assign,readwrite) SomeType position;

@end

void example (MyClass *x)
{
  const SomeType SomeTypeZero = {0.0f};

  x.position= SomeTypeZero;
}

