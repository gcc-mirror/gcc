/* { dg-do compile } */

/* This is an alternative to the original pr70740.c testcase,
   arrays are now 1 in size where they were 0 in the other testcase. */

extern int foo (void);
extern void *memcpy (void *, const void *, __SIZE_TYPE__);

struct
{
  char a[6];
} d;
struct
{
  int a1[1];
  int a2[1];
  int a3[1];
  int a4[1];
} a, c;
int b;

int *
bar ()
{
  if (b)
    return a.a4;
  return a.a2;
}

void
baz ()
{
  int *e, *f;
  if (foo ())
    e = c.a3;
  else
    e = c.a1;
  memcpy (d.a, e, 6);
  f = bar ();
  memcpy (d.a, f, 1);
}
