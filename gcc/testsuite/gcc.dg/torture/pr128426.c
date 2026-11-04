/* { dg-do compile } */

int a;
static void c (int);

void
d (void)
{
  int e = 0;
  void *f[] = { &&g, &&h, &&i };
  goto *f[a];
h:
  goto k;
i:
  e = a;
  goto l;
k:
  c (e);
  goto m;
g:
l:
  if (e)
    goto m;
  goto h;
m:;
}

static void
c (int p)
{
  if (p)
    d ();
}
