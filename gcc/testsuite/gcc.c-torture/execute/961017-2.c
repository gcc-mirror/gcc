void abort (void);
void exit (int);

int
main (void)
{
  int i = 0;


  if (sizeof (unsigned long int) == 4)
    {
      unsigned long int z = 0;

      do {
	z -= 0x00004000;
	i++;
	if (i > 0x00040000)
	  abort ();
      } while (z > 0);
      exit (0);
    }
  else if (sizeof (unsigned int) == 4)
    {
      unsigned int z = 0;

      do {
	z -= 0x00004000;
	i++;
	if (i > 0x00040000)
	  abort ();
      } while (z > 0);
      exit (0);
    }
  else
    exit (0);
}
