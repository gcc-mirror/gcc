/* PR 36513: -Wlogical-op warns about strchr */
/* { dg-do compile } */
/* { dg-options "-Wlogical-op" } */
#ifdef __cplusplus
#include <cstring>
#else 
#include <string.h>
#endif
int main2 ()
{
  char *s, t;
  strchr (s, t);
  return 0;
}
