// Check if the '- .cxx_construct' and '-.cxx_destruct' methods get called
// and if they perform their desired function.

// { dg-do run }
// { dg-options "-fobjc-call-cxx-cdtors" }

#include <objc/Object.h>
#include <stdlib.h>
#define CHECK_IF(expr) if(!(expr)) abort()

static int ctor1_called, ctor2_called, dtor1_called;

struct bar {
  int a, b;
  bar(void) {
    a = 5; b = 6;
    ctor1_called++;
  }
  ~bar(void) {
    a = b = 99;
    dtor1_called++;
  }
};

struct boo: bar {
  int c;
  boo(int _c = 9): c(_c) {
    ctor2_called++;
  }
};

@interface Baz: Object {
@public
  bar aa;
}
@end

@implementation Baz
@end

@interface Foo: Baz {
@public
  int a;
  boo bb;
  bar b;
  float c;
  bar d;
}
@end

@implementation Foo
@end

int main (void)
{
  CHECK_IF(!ctor1_called && !ctor2_called && !dtor1_called); /* are we sane? */

  Baz *baz = [Baz new];
  CHECK_IF(ctor1_called && !ctor2_called && !dtor1_called);
  CHECK_IF(baz->aa.a == 5 && baz->aa.b == 6);
  ctor1_called = 0;  /* reset */
  
  [baz free];
  CHECK_IF(!ctor1_called && !ctor2_called && dtor1_called);
  dtor1_called = 0;  /* reset */

  Foo *foo = [Foo new];
  CHECK_IF(ctor1_called && ctor2_called && !dtor1_called);
  CHECK_IF(foo->bb.a == 5 && foo->bb.b == 6 && foo->bb.c == 9);
  CHECK_IF(foo->b.a == 5 && foo->b.b == 6);
  CHECK_IF(foo->d.a == 5 && foo->d.b == 6);
  ctor1_called = ctor2_called = 0;  /* reset */
  
  [foo free];
  CHECK_IF(!ctor1_called && !ctor2_called && dtor1_called);
}
