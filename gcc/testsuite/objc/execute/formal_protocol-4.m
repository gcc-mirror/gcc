/* Contributed by Nicola Pero - Fri Mar  9 21:35:47 CET 2001 */
#include <objc/objc.h>
#include <objc/Object.h>

/* Test defining a protocol, a class adopting it in a category */

@protocol Evaluating
- (int) importance;
@end

@interface Feature : Object
@end

@implementation Feature
@end

@interface Feature (EvaluatingProtocol) <Evaluating>
@end

@implementation Feature (EvaluatingProtocol)
- (int) importance
{
  return 1000;
}
@end

int main (void)
{
  id <Evaluating> object;

  object = [Feature new];

  if ([object importance] != 1000)
    {
      abort ();
    }

  return 0;
}

