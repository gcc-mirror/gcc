/* { dg-do run } */
/* { dg-xfail-if "PR23614" { "*-*-*" } { "*" } { "-fnext-runtime" } } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */
#include "../objc-obj-c++-shared/Object1.h"
#include <stdlib.h>

class MyWidget {
 public:
  int a;
  MyWidget(void) { a = 17; }
};

MyWidget gWidget;

@protocol MyProto
- (MyWidget *)widget;
@end

@interface Foo: Object
@end

@interface Bar: Foo <MyProto>
@end

@interface Container: Object
+ (MyWidget *)elementForView:(Foo *)view;
@end

@implementation Foo
@end

@implementation Bar
- (MyWidget *)widget {
  return &gWidget;
}
@end

@implementation Container
+ (MyWidget *)elementForView:(Foo *)view
{
    MyWidget *widget = nil;
    if ([view conformsTo:@protocol(MyProto)]) {
        widget = [(Foo <MyProto> *)view widget];
    }
    return widget;
}
@end

int main(void) {
  id view = [Bar new];
  MyWidget *w = [Container elementForView: view];

  if (!w || w->a != 17)
    abort ();

  return 0;
}

#include "../objc-obj-c++-shared/Object1-implementation.h"
