/* Verify that calls to printf don't get eliminated even if their
   result on success can be computed at compile time (they can fail).
   The calls can still be transformed into those of other functions.
   { dg-require-effective-target unwrapped }
   { dg-skip-if "requires io" { freestanding } } */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

__attribute__ ((noipa)) void
write_file (void)
{
  printf ("1");
  printf ("%c", '2');
  printf ("%c%c", '3', '4');
  printf ("%s", "5");
  printf ("%s%s", "6", "7");
  printf ("%i", 8);
  printf ("%.1s\n", "9x");
}


int main (void)
{
  char *tmpfname = tmpnam (0);
  FILE *f = freopen (tmpfname, "w", stdout);
  if (!f)
    {
      perror ("fopen for writing");
      return 1;
    }

  write_file ();
  fclose (f);

  f = fopen (tmpfname, "r");
  if (!f)
    {
      perror ("fopen for reading");
      remove (tmpfname);
      return 1;
    }

  char buf[12] = "";
  if (1 != fscanf (f, "%s", buf))
    {
      perror ("fscanf");
      fclose (f);
      remove (tmpfname);
      return 1;
    }

  fclose (f);
  remove (tmpfname);

  if (strcmp (buf, "123456789"))
    abort ();

  return 0;
}
