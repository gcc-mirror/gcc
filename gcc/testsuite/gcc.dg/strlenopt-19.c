/* { dg-do run } */
/* { dg-options "-O2 -fdump-tree-strlen" } */

#include "strlenopt.h"

__attribute__((noinline, noclone)) char *
fn1 (int x, int y, int z)
{
  static char buf[40];
  const char *p;
  switch (x)
    {
    case 0:
      p = "abcd";
      /* Prevent cswitch optimization.  */
      asm volatile ("" : : : "memory");
      break;
    case 1:
      p = "efgh";
      break;
    case 2:
      p = "ijkl";
      break;
    default:
      p = "mnop";
      break;
    }
  if (y)
    {
      strcpy (buf, p);
      if (z)
	strcat (buf, "ABCDEFG");
      else
	strcat (buf, "HIJKLMN");
    }
  else
    {
      strcpy (buf, p + 1);
      if (z)
	strcat (buf, "OPQ");
      else
	strcat (buf, "RST");
    }
  return buf;
}

int
main ()
{
  int i;
  for (i = 0; i < 5; i++)
    {
      const char *p = "abcdefghijklmnop" + (i < 3 ? i : 3) * 4;
      const char *q;
      fn1 (i ? 0 : 1, 1, 1);
      q = fn1 (i, 0, 0);
      if (memcmp (q, p + 1, 3) != 0 || memcmp (q + 3, "RST", 4) != 0)
	abort ();
      fn1 (i ? 0 : 1, 0, 1);
      q = fn1 (i, 1, 0);
      if (memcmp (q, p, 4) != 0 || memcmp (q + 4, "HIJKLMN", 8) != 0)
	abort ();
      fn1 (i ? 0 : 1, 1, 0);
      q = fn1 (i, 0, 1);
      if (memcmp (q, p + 1, 3) != 0 || memcmp (q + 3, "OPQ", 4) != 0)
	abort ();
      fn1 (i ? 0 : 1, 0, 0);
      q = fn1 (i, 1, 1);
      if (memcmp (q, p, 4) != 0 || memcmp (q + 4, "ABCDEFG", 8) != 0)
	abort ();
    }
  return 0;
}

/* { dg-final { scan-tree-dump-times "strlen \\(" 0 "strlen" } } */
/* { dg-final { scan-tree-dump-times "memcpy \\(" 6 "strlen" } } */
/* { dg-final { scan-tree-dump-times "strcpy \\(" 0 "strlen" } } */
/* { dg-final { scan-tree-dump-times "strcat \\(" 0 "strlen" } } */
/* { dg-final { scan-tree-dump-times "strchr \\(" 0 "strlen" } } */
/* { dg-final { scan-tree-dump-times "stpcpy \\(" 0 "strlen" } } */
/* { dg-final { cleanup-tree-dump "strlen" } } */
