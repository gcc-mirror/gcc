/* PR middle-end/46499 */
/* { dg-do run } */
/* { dg-options "-O -fno-omit-frame-pointer -fno-tree-ccp -fno-tree-dominator-opts -finline-small-functions" } */

extern void abort (void);

int count = 0;

int
foo (void)
{
  count++;
  return 0;
}

int
bar (void)
{
  count++;
  return 0;
}

int
main ()
{
  if ((foo () == 1) & (bar () == 1))
    abort ();
  if (count != 2)
    abort ();
  return 0;
}
