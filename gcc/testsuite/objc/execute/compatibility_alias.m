/* Contributed by Nicola Pero - Thu Mar  8 17:23:59 CET 2001 */
#include <objc/objc.h>
#include <objc/Object.h>

@compatibility_alias MyObject Object;

int main (void)
{
  MyObject *object = [MyObject alloc];

  return 0;
}
