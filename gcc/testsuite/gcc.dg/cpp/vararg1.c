/* Test for changed behavior of the GNU varargs extension.
   ##args, where args is a rest argument which received zero tokens,
   used to delete the previous sequence of nonwhitespace characters.
   Now it deletes the previous token.  */

/* { dg-do run } */
/* { dg-options -w } */

#include <string.h>

#define S(str, args...) "  " str "\n", ##args

int
main()
{
  const char *s = S("foo");
  return strchr (s, '\n') == NULL;
}

