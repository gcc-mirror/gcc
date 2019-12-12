/* PR target/34225 */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-strict-aliasing -fstack-protector" } */
/* { dg-require-effective-target fstack_protector } */
/* { dg-require-effective-target size20plus } */

typedef __SIZE_TYPE__ size_t;
extern int sscanf (const char *, const char *, ...);
struct C
{
  char c[240];
};
struct O
{
  char **o;
};
struct P
{
  int p1;
  char p2[256];
  char p3[256];
  char p4[256];
};

extern void *s;
extern int f1 (char *, struct C *);
extern void f2 (void *, const char *);
extern void f3 (char *, const char *);
extern int f4 (int, double *, int);

int
foo (char *a[], struct O *b, int c, int d, struct P *e, int f, int *h,
     char *l)
{
  int i, g = 7, m = 0, n;
  struct C j[150];
  double *k[150];
  char o[100];

  if (f1 (o, &j[g]) < 0)
    g++;

  while (!m)
    {
      f2 (s, "xxx");
      for (i = 0; i < f; i++)
	if ((!b->o && __builtin_strncmp (l, "abcde", 5) == 0)
	    || (b->o && !b->o[c]))
	  {
	    *e[d].p4 = *e[d].p3 = *e[d].p2 = 0;
	    sscanf (l, "%s %s %[^\n]", e[d].p3, e[d].p2, e[d].p4);
	  }
      for (n = 0; n < d; n++)
	for (i = 0; i < g; i++)
	  {
	    f3 (a[i + 1], "foo");
	    if (f4 (h[i], k[i], e[n].p1) < 0)
	      f3 (a[i + 1], "bar");
	  }
    }
  return 0;
}
