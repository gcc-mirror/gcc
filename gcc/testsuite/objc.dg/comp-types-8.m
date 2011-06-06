/* { dg-do compile } */
/* Another gimplifier ICE... */

#include "../objc-obj-c++-shared/TestsuiteObject.h"

@interface MyView: TestsuiteObject {
  int _frame;
}
- (void)_finalize;
@end

@interface MyViewTemplate: MyView {
  void *_className;
}
- (id)createRealObject;
@end

@implementation MyViewTemplate
- (id)createRealObject {
    id realObj;
    *(MyView *)realObj = *(MyView *)self;
    return realObj;
}
@end
