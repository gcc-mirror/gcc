#include <stdio.h>
#include <string.h>

static int w[2];
static char * bytes;

int main(void)
{
  printf ("+++Endian test:\n");
  if (sizeof (int) == 2)
    {
      w[0] = 0x4142;
      w[1] = 0;
      bytes = (char *) w;
      if (strcmp(bytes, "AB") == 0)
	printf ("big endian\n");
      else if (strcmp(bytes, "BA") == 0)
	printf ("little endian\n");
      else
	{
	  printf ("nor big nor little endian\n");
	  return 1;
	}
    }
  else if (sizeof (int) == 4)
    {
      w[0] = 0x41424344;
      w[1] = 0;
      bytes = (char *) w;
      if (strcmp(bytes, "ABCD") == 0)
	printf ("big endian\n");
      else if (strcmp(bytes, "DCBA") == 0)
	printf ("little endian\n");
      else
	{
	  printf ("nor big nor little endian\n");
	  return 1;
	}
    }
  else
    {
      printf ("unexpected size of int\n");
      return 1;
    }
  return 0;
}
