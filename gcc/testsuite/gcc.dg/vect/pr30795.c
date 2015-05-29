/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

char bigDone[260];
int runningOrder[260];

int
main()
{
  int i;
  for (i = 0; i <= 255; i++) {
      bigDone [i] = ((char)0);
      runningOrder[i] = i;
  }
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
