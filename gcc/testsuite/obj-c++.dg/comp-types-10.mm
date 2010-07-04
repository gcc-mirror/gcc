/* Yet another mysterious gimplifier crasher.  */
/* { dg-do compile } */
/* { dg-prune-output ".*internal compiler error.*" } */
/* { dg-options "-O3" } */

@class NSString;
@protocol NSObject
@end
@interface NSObject <NSObject> {
}
@end
void __setRetained(id *ivar, id value) {
    *ivar = value;
}
static NSString *_logProcessPrefix = 0;
@implementation NSObject (ScopeAdditions)
+ (void)setObjectLogProcessPrefix:(NSString *)processPrefix {
    __setRetained(&_logProcessPrefix, processPrefix);
}
@end
