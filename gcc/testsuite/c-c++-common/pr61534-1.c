/* PR c/61534 */
/* { dg-options "-Wlogical-op" } */

extern int xxx;
#define XXX !xxx
int
test (void)
{
  if (XXX && xxx) /* { dg-bogus "logical" } */
    return 4;
  else
    return 0;
}
