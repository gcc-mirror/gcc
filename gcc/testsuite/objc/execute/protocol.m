@protocol Foo
- (void)foo;
@end

@interface Foo_c <Foo>
{
}
- (void)foo;
@end

@implementation Foo_c
- (void)foo
{
}
@end

int main (void)
{
  return 0;
}

