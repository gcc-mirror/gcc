/* Test for usage of namespace inside @implementation. */
/* { dg-do compile } */
@interface MyDocument
@end

@implementation MyDocument

// This deprecated usage works
static void foo1() { }

// This preferred usage does _not_ work
namespace
    {
    void foo2() { }
    }

namespace STD 
    {
	void foo3 () {}
    }

using namespace STD;

- (void) GARF {
  foo2();
  foo3();
}

@end
