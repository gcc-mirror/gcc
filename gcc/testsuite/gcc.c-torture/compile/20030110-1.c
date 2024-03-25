extern char bpp;
int inb(int);

void foo()
{
  if (bpp == 32)
    {
      if (2 < 8)
	{
	  do
	    {
	      while (inb(0x9ae8) & (0x0100 >> (2 +1)));
	    }
	  while(0);
	}
      else
	{
	  do
	    {
	      while (inb(0x9ae8) & (0x0100 >> (2)));
	    }
	  while(0);
	}
    }
  else
    do
      { 
	while (inb(0x9ae8) & (0x0100 >> (1)));
      }
    while(0);
  if (8 < 8)
    {
      do
	{
	  while (inb(0x9ae8) & (0x0100 >> (8 +1)));
	}
      while(0);
    }
}

