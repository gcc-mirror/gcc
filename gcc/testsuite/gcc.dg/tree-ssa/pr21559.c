/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1-details" } */

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
/* { dg-final { scan-tree-dump-times "Simplified relational" 1 "vrp1" } } */

/* Second, we should thread the edge out of the loop via the break
   statement.  We also realize that the final bytes == 0 test is useless,
   and thread over it.  We also know that toread != 0 is useless when
   entering while loop and thread over it.  */
/* { dg-final { scan-tree-dump-times "Threaded jump" 3 "vrp1" } } */


