/* PR middle-end/40172 */
/* { dg-do compile } */
/* { dg-options "-Wall -W -Werror -Wlogical-op" } */

struct rtx_def;
typedef struct rtx_def *rtx; 

extern int foo;
extern int bar;
extern int xxx;

int
test (void)
{
  if (((rtx) 0 != (rtx) 0) && xxx ? foo : bar)
    return 1;
  else if ((foo & 0) && xxx)
    return 2;
  else if (foo & 0)
    return 3;
  else if (0 && xxx)
    return 4;
  else if (0)
    return 5;
  if (((int) 0 != (int) 0) && bar ? foo : xxx)
    return 6;
  else if (0 != 0 && foo ? xxx : bar)
    return 7;
  else
    return 0;
}
