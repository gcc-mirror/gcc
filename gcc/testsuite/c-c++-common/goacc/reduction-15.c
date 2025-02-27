/* { dg-additional-options "-fdump-tree-gimple" } */
/* struct reductions.  */

typedef struct { int x, y; } int_pair;
typedef struct { float m, n; } flt_pair;
typedef struct
{
  int i;
  double d;
  float f;
  int a[4];
  int_pair ip;
  flt_pair fp;
} rectype;

#define n 1000

int
main(void)
{
  int i;
  rectype result, array[n];

  /* '+' reductions.  */
#pragma acc parallel
#pragma acc loop gang worker vector reduction (+:result)
  for (i = 0; i < n; i++)
    {
      result.i += array[i].i;
      result.f += array[i].f;
      result.ip.x += array[i].ip.x;
      result.ip.y += array[i].ip.y;
    }

  /* '*' reductions.  */
#pragma acc parallel
#pragma acc loop gang worker vector reduction (*:result)
  for (i = 0; i < n; i++)
    {
      result.i *= array[i].i;
      result.f *= array[i].f;
      result.ip.x *= array[i].ip.x;
      result.ip.y *= array[i].ip.y;
    }

  return 0;
}

/* Check that default copy maps are generated for loop reductions.  */
/* { dg-final { scan-tree-dump-times "map\\(tofrom:result \\\[len: \[0-9\]+\\\] \\\[runtime_implicit\\\]\\)" 2 "gimple" } } */

