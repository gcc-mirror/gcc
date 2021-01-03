/* Yet another stray infinite loop... */
/* { dg-do compile } */
// { dg-additional-options "-Wno-objc-root-class" }

@interface t
{
}
- (void)go;
@end
@implementation t
- (void)go
{
        }
} /* { dg-error "stray .\}. between Objective\\-C\\+\\+ methods" } */
@end

