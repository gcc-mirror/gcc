/* { dg-do run } */
/* { dg-options "-std=c99" } */
#include <stdlib.h>
#include <string.h>

#define str(t) #t

int main (void)
{
  const char s[] = str (ゲ);

  if (strcmp (s, "ゲ") != 0)
    abort ();
  
  return 0;
}
