/* Two of these types will, on current gcc targets, have the same
   mode but have different alias sets.  DOIT tries to get gcse to
   invalidly hoist one of the values out of the loop.  */

typedef int T0;
typedef long T1;
typedef long long T2;

int
doit(int sel, int n, void *p)
{
  T0 * const p0 = p;
  T1 * const p1 = p;
  T2 * const p2 = p;

  switch (sel)
    {
    case 0:
      do 
	*p0 += *p0;
      while (--n);
      return *p0 == 0;

    case 1:
      do 
	*p1 += *p1;
      while (--n);
      return *p1 == 0;

    case 2:
      do 
	*p2 += *p2;
      while (--n);
      return *p2 == 0;

    default:
      abort ();
    }
}

int
main()
{
  T0 v0; T1 v1; T2 v2;

  v0 = 1; doit(0, 5, &v0);
  v1 = 1; doit(1, 5, &v1);
  v2 = 1; doit(2, 5, &v2);

  if (v0 != 32) abort ();
  if (v1 != 32) abort ();
  if (v2 != 32) abort ();

  exit (0);
}
