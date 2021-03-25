/* Reduced from
   https://github.com/libguestfs/libguestfs/blob/e0a11061035d47b118c95706240bcc17fd576edc/tests/mount-local/test-parallel-mount-local.c#L299-L335
   which is GPLv2 or later.  */

#include <stdio.h>
#include <stdlib.h>

extern int foo (void);

void
test_mountpoint (const char *mp)
{
  const int nr_passes = 5 + (random () & 31);
  int pass;
  int ret = 1;
  FILE *fp;

  for (pass = 0; pass < nr_passes; ++pass) {
    if (foo ()) {
      goto error;
    }
    fp = fopen ("file", "w");
    if (fp == NULL) {
      goto error;
    }
    fprintf (fp, "hello world\n");
    fclose (fp); /* { dg-bogus "double 'fclose'" } */
  }

  ret = 0;

 error:
  exit (ret);
}
