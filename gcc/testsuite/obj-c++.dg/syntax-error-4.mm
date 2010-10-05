/* Yet another stray infinite loop... */
/* { dg-do compile } */

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

