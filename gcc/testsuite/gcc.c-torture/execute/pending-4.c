void abort (void);
void exit (int);

void dummy (x, y)
     int *x;
     int y;
{}

int
main (argc, argv)
     int argc;
     char **argv;
{
  int number_columns=9;
  int cnt0 = 0;
  int cnt1 = 0;
  int i,A1;

  for (i = number_columns-1; i != 0; i--)
    {         
      if (i == 1)
	{
	  dummy(&A1, i);
	  cnt0++;
	}
      else
	{
          dummy(&A1, i-1);
          cnt1++;
	}
    }
  if (cnt0 != 1 || cnt1 != 7)
    abort ();
  exit (0);
}
