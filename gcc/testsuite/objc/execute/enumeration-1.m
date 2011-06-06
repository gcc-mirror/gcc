/* Contributed by Nicola Pero -  Wed Dec  5 17:12:40 GMT 2001 */
#include <stdlib.h>
#import "../../objc-obj-c++-shared/TestsuiteObject.m"

/* Test using a bitfield enumeration ivar.  */

typedef enum
{
  black,
  white
} color;

@interface TestClass: TestsuiteObject
{
  color c:2;
}
- (color)color;
- (void)setColor: (color)a;
@end

@implementation TestClass
- (color)color
{
  return c;
}
- (void)setColor: (color)a
{
  c = a;
}
@end


int main (void)
{
  TestClass *c;
  
  c = [TestClass new];
  
  [c setColor: black];
  [c setColor: white];
  [c setColor: black];
  if ([c color] != black)
    {
      abort ();
    }
  

  return 0;
}

