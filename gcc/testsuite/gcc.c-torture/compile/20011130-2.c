/* This testcase caused infinite loop in life info computation
   after if conversion on IA-64.  Conditional register dead for
   pseudo holding sign-extended k was improperly computed,
   resulting in this pseudo being live at start of bb if it was
   dead at the end and vice versa; as it was a bb which had edge
   to itself, this resulted in alternative propagating this basic
   block forever.  */

typedef struct {
  unsigned char a;
  unsigned char b;
} S0;

typedef struct {
  S0 *c;
  int d;
  unsigned int e;
  unsigned char *f[3];
  void *g;
} S1;

int bar (int, void *);

int foo (S1 *x, float y)
{
  S0 *h;
  int i, j, k, l, m;
  float n, o, p;
  unsigned char *q, *r[3];

  h = x->c;
  m = h->a;
  l = h->b;
  n = y;
  o = 0.0;
  if (x->d == 8)
    for (j = 0; j < x->e; j++)
      for (k = 0; k < 3; k++)
	{
	  n = y;
	  o = 0.0;
	  if (m)
	    q = x->f[k] + x->e - 1 - j;
	  else
	    q = x->f[k] + j;
	  p = (*q - o) * y / (n - o);
	  p = 0.0 > p ? 0.0 : p;
	  p = y < p ? y : p;
	  if (l)
	    p = r[k][(int) p];
	  bar (p, x->g);
	}
  return 1;
}
