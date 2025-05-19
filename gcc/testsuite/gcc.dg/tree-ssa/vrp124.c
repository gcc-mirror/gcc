/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp" } */

/* Test removal of trailing zero mask ranges from signed values. */
/* Mask off the lower 4 bits of an integer. */
#define MASK 0XF

void dead (int c);
void keep();

/* A signed character should have a range something like : */
/* int [-INF, -16][0, 0][16, 2147483632] MASK 0xfffffff0 VALUE 0x0 */

int
foo2 (int c)
{
  c = c & ~MASK;
  if (c == 0)
    return 0;
  if (c > -16)
    {
      keep ();
      if (c < 16)
	dead (c);
    }
  if (c > (__INT_MAX__ & ~MASK))
    dead (c);
  return 0;
}

/* { dg-final { scan-tree-dump-not "dead" "evrp" } } */
