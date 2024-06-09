void abort (void);
void exit (int);

int
main (void)
{
  union
    {
      double d;
      unsigned short i[sizeof (double) / sizeof (short)];
    } u;
  int a = 0;
  int b = -5;
  int j;

  u.d = (double) a / b;

  /* Look for the right pattern, but be sloppy since
     we don't know the byte order.  */
  for (j = 0; j < sizeof (double) / sizeof (short); j++)
    {
      if (u.i[j] == 0x8000)
	exit (0);
    }
  abort ();
}
