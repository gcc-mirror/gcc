struct { char *m; long n; } a[20];
int b = 20, c;
void bar(void) __attribute__((__noreturn__));

int
foo(int x)
{
  int i;

  for (i = 0; i < x; i++)
    {
      a[0].m = "a"; a[0].n = 10; c=1;
      a[c].m = "b"; a[c].n = 32; c++;
      if (c >= b) bar ();
      a[c].m = "c"; a[c].n = 80; c++;
      if (c >= b) bar ();
    }
  return 0;
}
