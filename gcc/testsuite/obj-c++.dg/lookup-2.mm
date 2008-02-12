/* { dg-do run } */
/* { dg-xfail-if "PR23614" { "*-*-*" } { "*" } { "" } } */

#include <objc/Object.h>
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
