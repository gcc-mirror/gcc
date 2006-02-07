/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1" } */

static int blocksize = 4096;

int bar (int);

void foo (void)
{
  int toread;
  int bytes;
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

/* { dg-final { scan-tree-dump-times "Simplified relational" 1 "vrp1" } } */
/* { dg-final { cleanup-tree-dump "vrp1" } } */
