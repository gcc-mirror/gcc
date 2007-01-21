#include "interface-1.h"

@implementation TestClass
+ (int) test
{
  return 0;
}
@end

int main (void)
{
  return [TestClass test];
}

