/* { dg-do compile } */
/* { dg-options "-O3" } */
/* PR 14498.  */
#include <stdarg.h>
extern void baz(char *);
void bar (va_list *args)
{
  if (args)
    {
      char *s = va_arg(*args, char *);
      baz(s);
    }
}
