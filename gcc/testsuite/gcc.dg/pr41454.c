/* { dg-do run } */
/* { dg-options "-O2 -fno-tree-vrp" } */

extern void abort (void);

int main ()
{
  int BM_tab2[0400];
  int *BM_tab = BM_tab2;
  int *BM_tab_base;

  BM_tab_base = BM_tab;
  BM_tab += 0400;
  while (BM_tab_base != BM_tab)
    {
      *--BM_tab = 6;
      *--BM_tab = 6;
    }
  if (BM_tab2[0] != 6)
    abort ();
  return 0;
}
