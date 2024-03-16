int
main (void)
{
  typedef short int xtype;

  xtype i;
  xtype ii;

  for (i = 0; i < 100; i++)
    for (ii = 65535; --ii;)
      ;
}
