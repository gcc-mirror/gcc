static int kind_varread(char *str)
{
  if (0 == memcmp("%_#",               str, 3))  return 2;
  /* NOTREACHED */
}

