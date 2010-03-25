/* Contributed by Nicola Pero -  Wed Dec  5 17:12:40 GMT 2001 */
#include <stdlib.h>
#import "../../objc-obj-c++-shared/Object1.h"
#include <objc/objc.h>

typedef enum { black, white } color;

typedef struct 
{
  color a:2;
  color b:2;
} color_couple;

@interface TestClass: Object
{
  color_couple *c;
}
- (color_couple *)colorCouple;
- (void)setColorCouple: (color_couple *)a;
@end

@implementation TestClass
- (color_couple *)colorCouple
{
  return c;
}
- (void)setColorCouple: (color_couple *)a
{
  c = a;
}
@end


int main (void)
{
  color_couple cc;
  TestClass *c;
  
  c = [TestClass new];
  
  cc.a = black;
  cc.b = white;

  [c setColorCouple: &cc];
  if ([c colorCouple] != &cc)
    {
      abort ();
    }
  

  return 0;
}
