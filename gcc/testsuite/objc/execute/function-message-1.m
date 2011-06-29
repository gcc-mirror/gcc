#include "../../objc-obj-c++-shared/TestsuiteObject.m"

@interface Foo : TestsuiteObject
+ bar;
@end

int foocalled = 0;
int barcalled = 0;


id foo()
{
    if (foocalled)
      abort ();
    foocalled = 1;
    return [Foo class];
}

@implementation Foo
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
    [foo() bar];
    return 0;
}
