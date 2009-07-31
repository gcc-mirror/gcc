void foo (int *BM_tab)
{
  int *BM_tab_base;

  BM_tab_base = BM_tab;
  BM_tab += 0400;
  while (BM_tab_base != BM_tab)
    *--BM_tab = 6;
}

int main ()
{
  int BM_tab[0400];
  foo (BM_tab);
  return 0;
}
