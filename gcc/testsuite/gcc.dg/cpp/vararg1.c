/* { dg-do run } */
/* { dg-options -w } */

/* count() used to give 1 owing to a buggy test for varargs.  */
#define count(y...)  count1 ( , ##y)
#define count1(y...) count2 (y,1,0)
#define count2(_,x0,n,y...) n
#if count() != 0 || count(A) != 1
#error Incorrect vararg argument counts
#endif

/* Test for changed behavior of the GNU varargs extension.
   ##args, where args is a rest argument which received zero tokens,
   used to delete the previous sequence of nonwhitespace characters.
   Now it deletes the previous token.  */

#include <string.h>

#define S(str, args...) "  " str "\n", ##args

int
main()
{
  const char *s = S("foo");
  return strchr (s, '\n') == NULL;
}
