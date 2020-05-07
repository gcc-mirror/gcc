/* PR middle-end/40172 */
/* { dg-do compile } */
/* { dg-xfail-if "" { "*-*-*" } } */
/* { dg-options "-Wall -W -Werror -Wlogical-op" } */

extern int xxx;

#define XXX xxx

int
test (void)
{
  if (!XXX && xxx)
    return 4;
  else
    return 0;
}
