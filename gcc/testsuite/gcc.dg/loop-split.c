/* { dg-do run } */
/* { dg-options "-O2 -fsplit-loops -fdump-tree-lsplit-details" } */
/* { dg-require-effective-target int32plus } */

#ifdef __cplusplus
extern "C" int printf (const char *, ...);
extern "C" void abort (void);
#else
extern int printf (const char *, ...);
extern void abort (void);
#endif

/* Define TRACE to 1 or 2 to get detailed tracing.
   Define SINGLE_TEST to 1 or 2 to get a simple routine with
   just one loop, called only one time or with multiple parameters,
   to make debugging easier.  */
#ifndef TRACE
#define TRACE 0
#endif

#define loop(beg,step,beg2,cond1,cond2) \
    do \
      { \
	sum = 0; \
        for (i = (beg), j = (beg2); (cond1); i+=(step),j+=(step)) \
          { \
            if (cond2) { \
	      if (TRACE > 1) printf ("a: %d %d\n", i, j); \
              sum += a[i]; \
	    } else { \
	      if (TRACE > 1) printf ("b: %d %d\n", i, j); \
              sum += b[i]; \
	    } \
          } \
	if (TRACE > 0) printf ("sum: %d\n", sum); \
	check = check * 47 + sum; \
      } while (0)

#ifndef SINGLE_TEST
unsigned __attribute__((noinline, noclone)) dotest (int beg, int end, int step,
					       int c, int *a, int *b, int beg2)
{
  unsigned check = 0;
  int sum;
  int i, j;
  loop (beg, 1, beg2, i < end, j < c);
  loop (beg, 1, beg2, i <= end, j < c);
  loop (beg, 1, beg2, i < end, j <= c);
  loop (beg, 1, beg2, i <= end, j <= c);
  loop (beg, 1, beg2, i < end, j > c);
  loop (beg, 1, beg2, i <= end, j > c);
  loop (beg, 1, beg2, i < end, j >= c);
  loop (beg, 1, beg2, i <= end, j >= c);
  beg2 += end-beg;
  loop (end, -1, beg2, i >= beg, j >= c);
  loop (end, -1, beg2, i >= beg, j > c);
  loop (end, -1, beg2, i > beg, j >= c);
  loop (end, -1, beg2, i > beg, j > c);
  loop (end, -1, beg2, i >= beg, j <= c);
  loop (end, -1, beg2, i >= beg, j < c);
  loop (end, -1, beg2, i > beg, j <= c);
  loop (end, -1, beg2, i > beg, j < c);
  return check;
}

#else

int __attribute__((noinline, noclone)) f (int beg, int end, int step,
					  int c, int *a, int *b, int beg2)
{
  int sum = 0;
  int i, j;
  //for (i = beg, j = beg2; i < end; i += 1, j++ /*step*/)
  for (i = end, j = beg2 + (end-beg); i > beg; i += -1, j-- /*step*/)
    {
      // i - j == X --> i = X + j
      // --> i < end == X+j < end == j < end - X
      // --> newend = end - (i_init - j_init)
      // j < end-X && j < c --> j < min(end-X,c)
      // j < end-X && j <= c --> j <= min(end-X-1,c) or j < min(end-X,c+1{OF!})
      //if (j < c)
      if (j >= c)
	printf ("a: %d %d\n", i, j);
      /*else
	printf ("b: %d %d\n", i, j);*/
	/*sum += a[i];
      else
	sum += b[i];*/
    }
  return sum;
}

int __attribute__((noinline, noclone)) f2 (int *beg, int *end, int step,
					  int *c, int *a, int *b, int *beg2)
{
  int sum = 0;
  int *i, *j;
  for (i = beg, j = beg2; i < end; i += 1, j++ /*step*/)
    {
      if (j <= c)
	printf ("%d %d\n", i - beg, j - beg);
	/*sum += a[i];
      else
	sum += b[i];*/
    }
  return sum;
}
#endif

extern int printf (const char *, ...);

int main ()
{
  int a[] = {0,0,0,0,0, 1,2,3,4,5,6,7,8,9,          0,0,0,0,0};
  int b[] = {0,0,0,0,0, -1,-2,-3,-4,-5,-6,-7,-8,-9, 0,0,0,0,0,};
  int c;
  int diff = 0;
  unsigned check = 0;
#if defined(SINGLE_TEST) && (SINGLE_TEST == 1)
  //dotest (0, 9, 1, -1, a+5, b+5, -1);
  //return 0;
  f (0, 9, 1, 5, a+5, b+5, -1);
  return 0;
#endif
  for (diff = -5; diff <= 5; diff++)
    {
      for (c = -1; c <= 10; c++)
	{
#ifdef SINGLE_TEST
	  int s = f (0, 9, 1, c, a+5, b+5, diff);
	  //int s = f2 (a+0, a+9, 1, a+c, a+5, b+5, a+diff);
	  printf ("%d ", s);
#else
	  if (TRACE > 0)
	    printf ("check %d %d\n", c, diff);
	  check = check * 51 + dotest (0, 9, 1, c, a+5, b+5, diff);
#endif
	}
      //printf ("\n");
    }
  //printf ("%u\n", check);
  if (check != 3213344948)
    abort ();
  return 0;
}

/* All 16 loops in dotest should be split.  */
/* { dg-final { scan-tree-dump-times "Loop split" 16 "lsplit" } } */
