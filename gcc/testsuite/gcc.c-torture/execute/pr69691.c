/* PR rtl-optimization/69691 */

char u[] = { 46, 97, 99, 104, 52, 0 };
char *v[] = { u, 0 };
struct S { char a[10]; struct S *b[31]; };
struct S r[7], *r2 = r;
static struct S *w = 0;

__attribute__((noinline, noclone)) int
fn (int x)
{
  if (__builtin_strchr (u, x) || x == 96)
    return x;
  __builtin_abort ();
}

__attribute__((noinline, noclone)) int
foo (char x)
{
  if (x == 0)
    __builtin_abort ();
  if (fn (x) >= 96 && fn (x) <= 122)
    return (fn (x) - 96);
  else if (x == 46)
    return 0;
  else
    {
      __builtin_printf ("foo %d\n", x);
      return -1;
    }
}

__attribute__((noinline, noclone)) void
bar (char **x)
{
  char **b, c, *d, e[500], *f, g[10];
  int z, l, h, i;
  struct S *s;

  w = r2++;
  for (b = x; *b; b++)
    {
      __builtin_strcpy (e, *b);
      f = e;
      do
	{
	  d = __builtin_strchr (f, 32);
	  if (d)
	    *d = 0;
	  l = __builtin_strlen (f);
	  h = 0;
	  s = w;
	  __builtin_memset (g, 0, sizeof (g));
	  for (z = 0; z < l; z++)
	    {
	      c = f[z];
	      if (c >= 48 && c <= 57)
		g[h] = c - 48;
	      else
		{
		  i = foo (c);
		  if (!s->b[i])
		    {
		      s->b[i] = r2++;
		      if (r2 == &r[7])
			__builtin_abort ();
		    }
		  s = s->b[i];
		  h++;
		}
	    }
	  __builtin_memcpy (s->a, g, 10);
	  if (d)
	    f = d + 1;
	}
      while (d);
    }
}

__attribute__((noinline, noclone)) void
baz (char *x)
{
  char a[300], b[300];
  int z, y, t, l;
  struct S *s;

  l = __builtin_strlen (x);
  *a = 96;
  for (z = 0; z < l; z++)
    {
      a[z + 1] = fn ((unsigned int) x[z]);
      if (foo (a[z + 1]) <= 0)
	return;
    }
  a[l + 1] = 96;
  l += 2;
  __builtin_memset (b, 0, l + 2);

  if (!w)
    return;

  for (z = 0; z < l; z++)
    {
      s = w;
      for (y = z; y < l; y++)
	{
	  s = s->b[foo (a[y])];
	  if (!s)
	    break;
	  for (t = 0; t <= y - z + 2; t++)
	    if (s->a[t] > b[z + t])
	      b[z + t] = s->a[t];
	}
    }
  for (z = 3; z < l - 2; z++)
    if ((b[z] & 1) == 1)
     asm ("");
}

int
main ()
{
  bar (v);
  char c[] = { 97, 97, 97, 97, 97, 0 };
  baz (c);
  return 0;
}
