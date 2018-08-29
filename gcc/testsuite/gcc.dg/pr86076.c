/* { dg-do compile { target pthread } }  */
/* { dg-options "-O2 -ftree-parallelize-loops=2 -fno-tree-dce -fno-tree-pre -fno-tree-vrp --param max-loop-header-insns=1" } */

int __attribute__ ((noinline))
lv (int tm)
{
  (void) tm;

  return 0;
}

void
o7 (int uu)
{
  while (uu < 1)
    while (uu != 0)
      {
	short int ca;

	ca = lv (0);
	(void) ca;
	++uu;
      }

  lv (lv (0));
}
