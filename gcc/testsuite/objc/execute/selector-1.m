/* Contributed by Nicola Pero - Thu Mar  8 16:27:46 CET 2001 */
#include <objc/objc.h>
#include <objc/objc-api.h>
#include <objc/Object.h>

int main (void)
{
  SEL selector;
  char *selname;

  selector = @selector (alloc);
#ifdef __NEXT_RUNTIME__
  selname = sel_getName (selector);
#else
  selname = sel_get_name (selector);
#endif
  if (strcmp (selname, "alloc"))
    abort ();

  return 0;
}
