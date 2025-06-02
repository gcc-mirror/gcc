/* { dg-do run } */
/* { dg-options "-std=c2y -pedantic-errors" } */

#include <stdcountof.h>

#define assert(e)  ((e) ? (void) 0 : __builtin_abort ())

extern int strcmp (const char *, const char *);

#ifndef countof
#error "countof not defined"
#endif

int a[3];
int b[countof a];

#define str(x) #x
#define xstr(x) str(x)

int
main (void)
{
  assert (strcmp (xstr(countof), "_Countof") == 0);
}
