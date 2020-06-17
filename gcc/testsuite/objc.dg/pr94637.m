/* PR objc/94637 */
/* { dg-do compile } */

#include <objc/objc.h>

SEL
foo ()
{
  return @selector(example::);
}
