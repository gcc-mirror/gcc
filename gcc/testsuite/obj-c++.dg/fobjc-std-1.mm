/* Test warnings when using -fobjc-std=objc1.  */
/* { dg-do compile } */
/* { dg-options "-fobjc-std=objc1" } */

#include <objc/objc.h>

__attribute__ ((deprecated))
@interface MyRootClass /* { dg-error "class attributes are not available in Objective.C 1.0" } */
{
  Class isa; /* { dg-error ".@package. is not available in Objective.C 1.0" } */
@package
  int a;
  int b;
}
+ (id) alloc __attribute__ ((deprecated)); /* { dg-error "not available in Objective.C 1.0" } */
+ (id) name;
- (id) init;
- (id) testMe: (id) __attribute__((unused)) argument; /* { dg-error "not available in Objective.C 1.0" } */
@property (nonatomic) int a; /* { dg-error "not available in Objective.C 1.0" } */
@property (nonatomic) int b; /* { dg-error "not available in Objective.C 1.0" } */
@end

@implementation MyRootClass
+ (id) alloc { return self; }
+ (id) name { return self; }
- (id) init  { return self; }
- (id) testMe: (id) __attribute__((unused)) argument { return self; } /* { dg-error "not available in Objective.C 1.0" } */
@synthesize a; /* { dg-error "not available in Objective.C 1.0" } */
@dynamic b; /* { dg-error "not available in Objective.C 1.0" } */
@end

__attribute__ ((deprecated))
@protocol MyProtocol /* { dg-error "protocol attributes are not available in Objective.C 1.0" } */

@required /* { dg-error "not available in Objective.C 1.0" } */
- (id) variable __attribute__ ((deprecated)); /* { dg-error "not available in Objective.C 1.0" } */
@optional /* { dg-error "not available in Objective.C 1.0" } */
@end
#if 0 /* fast enumeration is not implemented even in Objective-C 2.0 */
@interface MyRootClass (NSFastEnumeration)
- (unsigned long)countByEnumeratingWithState: (struct __objcFastEnumerationState *)state
                                     objects:(id *)stackbuf 
                                       count:(unsigned int)len;
@end

@class NSArray;

int array_length (NSArray *array)
{
  int i = 0;

  for (id object in array) /*  dg-error "not available in Objective.C 1.0"  */
    i++;
      
  return i;
}
#endif

id test (void)
{
  return MyRootClass.name; /* { dg-error "not available in Objective.C 1.0" } */
}