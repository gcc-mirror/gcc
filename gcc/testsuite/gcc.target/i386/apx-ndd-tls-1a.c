/* PR target/113733 */
/* { dg-do assemble { target { apxf && { ! ia32 } } } } */
/* { dg-require-effective-target tls } */
/* { dg-options "-mapxf -O3 -w" } */

extern __thread int a, j;
enum b
{
  c,
  d
};
struct e
{
  long f;
  struct
  {
    char g[1024];
  };
} typedef h ();
long i;
int o (char *);
static enum b
k (int *p)
{
  h l;
  struct e n;
  do
    {
      l (n, n.f, p);
      char **m;
      for (; *m; ++m)
        if (o (*m))
          i = j;
    }
  while (d);
}
void
getgrouplist ()
{
  k (&a);
}
