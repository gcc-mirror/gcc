/* VLAs should be deallocated on a jump to before their definition,
   including a jump to a label in an inner scope.  PR 19771.  */
/* { dg-require-effective-target alloca } */

#if (__SIZEOF_INT__ <= 2)
#define LIMIT 10000
#else
#define LIMIT 1000000
#endif

void *volatile p;

int
main (void)
{
  int n = 0;
  if (0)
    {
    lab:;
    }
  int x[n % 1000 + 1];
  x[0] = 1;
  x[n % 1000] = 2;
  p = x;
  n++;
  if (n < LIMIT)
    goto lab;
  return 0;
}
