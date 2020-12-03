/* Contributed by Igor Seleznev <selez@mail.ru>.  */
/* This used to be broken.  */
/* { dg-additional-options "-Wno-objc-root-class" } */

#include <objc/objc.h>

@interface A
+ (A *)currentContext;
@end

@interface B
+ (B *)currentContext;
@end

int main()
{
    [A currentContext];  /* { dg-bogus "multiple declarations" }  */
    return 0;
}

@implementation A
+ (A *)currentContext { return nil; }
@end
@implementation B
+ (B *)currentContext { return nil; }
@end
