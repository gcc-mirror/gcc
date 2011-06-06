#include <stdlib.h>
#import "../../objc-obj-c++-shared/TestsuiteObject.m"

@interface Foo : TestsuiteObject
+ foo;
+ bar;
@end

int foocalled = 0;
int barcalled = 0;


@implementation Foo
+ foo
{
    if (foocalled)
      abort ();
    foocalled = 1;
    return self;
}
+ bar
{
    if (barcalled)
      abort ();
    barcalled = 1;
    return self;
}
@end

int main(int argc,char **argv)
{
    [[Foo foo] bar];
    return 0;
}
