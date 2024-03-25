void
f (int phaseone)
{
  typedef struct
    {
      unsigned char *p;
    }
  FILE;
  FILE b[2];
  static unsigned char xchr[2];
  int j;
  int for_end;
  if (phaseone)
    {
      if (j <= for_end)
	do
	  *(b[1].p) = xchr[j];
	while (j++ < 10);
    }
}
