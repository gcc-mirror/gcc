/* { dg-do run } */
/* { dg-options "-O" } */
extern unsigned long x1;

char *
f (char *a, char *b)
{
  return a;
}

int
g (char *a)
{
  return 2;
}

void
h (char *p[])
{
  char n[x1][512];
  char *l = f (p[1], " ");
  if (g (p[0]))
    n[0][0] = '\0';
  while (l && *l)
    {
    }
}

unsigned long x1;

int
main ()
{
  return 0;
}
