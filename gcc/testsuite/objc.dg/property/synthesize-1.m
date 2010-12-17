/* { dg-do compile } */

#include <objc/objc.h>

@interface MyRootClass
{
  Class isa;
}
@end

@implementation MyRootClass
@end

@synthesize isa;                    /* { dg-error ".@synthesize. not in @implementation context" } */

@interface Test : MyRootClass
{
  int v1;
  int v2;
  int v3;
  int v4;
  int v5;
  int v6;
  int v7;
  int v8;
}
@property int v1;
@property int v2;
@property int v3;
@property int v4;
@property int v5;
@property int v6;
@property int v7;
@property int v8;
@end

@implementation Test
@synthesize;                        /* { dg-error "expected identifier" } */
@synthesize v1, ;                   /* { dg-error "expected identifier" } */
@synthesize v2, v3 = ;              /* { dg-error "expected identifier" } */
@synthesize v4, v5=v6, v6 = v5,v7;
@synthesize v8;       
/* Some of the @synthesize above will fail due to syntax errors.  The
   compiler will then complain that the methods implementing the
   properties are missing.  That is correct, but we are not
   interested.  The following ones shut up the compiler.  */
- (int) v1 { return v1; }
- (void) setV1: (int)a { v1 = a; }
- (int) v2 { return v2; }
- (void) setV2: (int)a { v2 = a; }
- (int) v3 { return v3; }
- (void) setV3: (int)a { v3 = a; }
@end
