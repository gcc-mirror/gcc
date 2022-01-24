/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp-details" } */

static int blocksize = 4096;

int bar (int);

void foo (void)
{
  int toread;
  int bytes;
  __attribute__ ((used))
  static char eof_reached = 0;

  toread = blocksize;
  bytes = 1;

  while (toread != 0)
    {
      bytes = bar (toread);
      if (bytes <= 0)
        {
          if (bytes < 0)
            continue;
          break;
        }
      toread -= bytes;
    }

  if (bytes == 0)
    eof_reached = 1;
}


/* First, we should simplify the bits < 0 test within the loop.  */
/* { dg-final { scan-tree-dump-times "Simplified relational" 1 "evrp" } } */
