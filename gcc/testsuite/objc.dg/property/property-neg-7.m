/* { dg-do compile } */

@interface NSArray 
{
  int count;
}
@property(readonly) int count;
@end

@implementation NSArray
@synthesize count;
@end

void foo (NSArray *ans[], id pid, id apid[], int i) {
    NSArray *test;
    test.count = 1; /* { dg-error "readonly property cannot be set" } */
    ((NSArray *)pid).count = 1;  /* { dg-error "readonly property cannot be set" } */
    ((NSArray *)apid[i]).count = 1; /* { dg-error "readonly property cannot be set" } */
    ans[i].count = 3; /* { dg-error "readonly property cannot be set" } */
}
