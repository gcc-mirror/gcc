/* { dg-do run } */
/* { dg-options "-fdump-tree-original" } */

/* Check that MIN-MAX and MAX-MIN combinations are folded.  */

extern void abort (void);

#define MIN(a,b) ((a) < (b) ? (a) : (b))
#define MAX(a,b) ((a) > (b) ? (a) : (b))

int f1(int a, int b)
{
   return MIN (MAX (a, b), b); /* == b */
}

int f2(int a, int b)
{
   return MAX (MIN (a, b), b); /* == b */
}

int f3(int a, int b)
{
   return MIN (MAX (b, a), b); /* == b */
}

int f4(int a, int b)
{
   return MAX (MIN (b, a), b); /* == b */
}


int g1(int a, int b)
{
   return MIN (a, MAX (a, b)); /* == a */
}

int g2(int a, int b)
{
   return MAX (a, MIN (a, b)); /* == a */
}

int g3(int a, int b)
{
   return MIN (a, MAX (b, a)); /* == a */
}

int g4(int a, int b)
{
   return MAX (a, MIN (b, a)); /* == a */
}

int main(void)
{
  if (f1 (1, 2) != 2)
    abort ();

  if (f2 (1, 2) != 2)
    abort ();

  if (f3 (1, 2) != 2)
    abort ();

  if (f4 (1, 2) != 2)
    abort ();

  if (g1 (1, 2) != 1)
    abort ();

  if (g2 (1, 2) != 1)
    abort ();

  if (g3 (1, 2) != 1)
    abort ();

  if (g4 (1, 2) != 1)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "MIN_EXPR" 0 "original"} } */
/* { dg-final { scan-tree-dump-times "MAX_EXPR" 0 "original"} } */
