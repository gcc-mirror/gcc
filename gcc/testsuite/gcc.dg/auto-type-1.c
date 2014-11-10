/* Test __auto_type.  Test correct uses.  */
/* { dg-do run } */
/* { dg-options "" } */
/* { dg-require-effective-target alloca } */

extern void abort (void);
extern void exit (int);

__auto_type i = 1;
extern int i;
__auto_type c = (char) 1;
extern char c;
static __auto_type u = 10U;
extern unsigned int u;
const __auto_type ll = 1LL;
extern const long long ll;

int
main (void)
{
  if (i != 1 || c != 1 || u != 10U)
    abort ();
  __auto_type ai = i;
  int *aip = &ai;
  if (ai != 1)
    abort ();
  __auto_type p = (int (*) [++i]) 0;
  if (i != 2)
    abort ();
  if (sizeof (*p) != 2 * sizeof (int))
    abort ();
  int vla[u][u];
  int (*vp)[u] = &vla[0];
  __auto_type vpp = ++vp;
  if (vp != &vla[1])
    abort ();
  exit (0);
}
