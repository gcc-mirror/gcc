int tab[9];
execute(oip, ip)
     unsigned short *oip, *ip;
{
#ifndef NO_LABEL_VALUES
  int x = 0;
  int *xp = tab;
base:
  x++;
  if (x == 4)
    {
      *xp = 0;
      return;
    }
  *xp++ = ip - oip;
  goto *(&&base + *ip++);
#else
  tab[0] = 0;
  tab[1] = 1;
  tab[2] = 2;
  tab[3] = 0;
#endif
}

main()
{
  unsigned short ip[10];
  int i;
  for (i = 0; i < 10; i++)
    ip[i] = 0;
  execute(ip, ip);
  if (tab[0] != 0 || tab[1] != 1 || tab[2] != 2 || tab[3] != 0)
    abort();
  exit(0);
}
