/* Contributed by Nicola Pero - Thu Mar  8 16:27:46 CET 2001 */
#include <objc/objc.h>
#include <objc/objc-api.h>
#include <objc/Object.h>

int main (void)
{
  SEL selector;

  selector = @selector (alloc);
  if (strcmp (sel_get_name (selector), "alloc"))
    {
      abort ();
    }

  return 0;
}
