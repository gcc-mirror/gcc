/* Contributed by Nicola Pero - Fri Aug 30 12:55:37 2002 */ 
#include <objc/objc.h>
#include <objc/Object.h>

/* Test that calling a method of a nil object results in
   nothing to happen (but not a crash), and nil to be
   returned.  */

@interface TestClass : Object

- (void) testVoid;
- (id) testId;

- (void) testVoidWithA: (int)a;
- (id) testIdWithA: (id)a;

- (void) testVoidWithA: (int)a  andB: (int)b;
- (id) testIdWithA: (id)g  andB: (id)b;

- (void) voidSum: (int)firstNumber, ...;
- (id) sum: (int)firstNumber, ...;

@end

int main (void)
{
  TestClass *t = nil;

  [t testVoid];

  if ([t testId] != nil)
    {
      abort ();
    }

  [t testVoidWithA: 234];

  if ([t testIdWithA: t] != nil)
    {
      abort ();
    }

  [t testVoidWithA: 12004  andB: -123];

  if ([t testIdWithA: t  andB: nil] != nil)
    {
      abort ();
    }


  [t voidSum: 1, -2, 3, -4, 5, -6, 7, -8, 9, -10, 
     11, -12, 13, -14, 15, -16, 17, -18, 19, -20];

  if ([t sum: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 
         11, 12, 13, 14, 15, 16, 17, 18, 19, 20] != nil)
    {
      abort ();
    }

  return 0;
}
