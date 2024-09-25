/* PR 36513: -Wlogical-op warns about strchr */
/* { dg-do compile } */
/* { dg-options "-Wlogical-op" } */
/* { dg-skip-if "requires hosted libstdc++ for cstring" { ! hostedlib } } */
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
