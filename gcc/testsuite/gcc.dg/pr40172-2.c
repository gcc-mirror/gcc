/* PR middle-end/40172 */
/* { dg-do compile } */
/* { dg-options "-Wall -W -Werror" } */

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
