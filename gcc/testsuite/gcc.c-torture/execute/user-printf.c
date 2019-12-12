/* Verify that calls to a function declared wiith attribute format (printf)
   don't get eliminated even if their result on success can be computed at
   compile time (they can fail).
   { dg-require-effective-target unwrapped }
   { dg-prune-output "warning: warning: \[^\n\r\]* possibly used unsafely" }
   { dg-skip-if "requires io" { avr-*-* } }
   { dg-skip-if "requires io" { freestanding } } */

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void __attribute__ ((format (printf, 1, 2), noipa))
user_print (const char *fmt, ...)
{
  va_list va;
  va_start (va, fmt);
  vfprintf (stdout, fmt, va);
  va_end (va);
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

  user_print ("1");
  user_print ("%c", '2');
  user_print ("%c%c", '3', '4');
  user_print ("%s", "5");
  user_print ("%s%s", "6", "7");
  user_print ("%i", 8);
  user_print ("%.1s\n", "9x");

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
