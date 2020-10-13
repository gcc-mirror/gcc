/* PR sanitizer/80166 */
/* { dg-do run } */
/* { dg-additional-options "-Wno-stringop-overflow" } */

#include <sys/types.h>
#include <unistd.h>

int
main (int argc, char **argv)
{
  gid_t groups;
  int r = getgroups (0, &groups);
  if (r < 0)
    __builtin_abort ();

  r = getgroups (-1, &groups);
  if (r != -1)
    __builtin_abort ();

  r = getgroups (-1, NULL);
  if (r != -1)
    __builtin_abort ();

  return 0;
}
