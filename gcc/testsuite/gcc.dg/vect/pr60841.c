/* { dg-do compile } */
/* { dg-additional-options "-ffast-math" } */

/* This testcase shouldn't consume much memory or produce a 1GB vectorizer
   dump file due to SLP tree explosion.  */

struct S { int f1, f2, f3, f4; } a;
struct T { short f3, f2, f1, f4; };
int b, c, d, e, f, g;
unsigned long z;

void
foo (struct T *p, struct T *q, int x, int w)
{
  for (; x; x++)
    {
      struct S h;
      int i;
      struct T j;
      struct T *r;
      h = a;
      g = 0;
      r = p + 2 * (c + 4) + 1;
      j = *r;
      r = p;
      f = r->f1 - 1;
      b = +1.0 + f * f;
      i = (r->f2 + j.f2) / 2;
      f = r->f3 - 1;
      b += 1.0 - i * f * f;
      f = r->f4 - 1;
      if (b)
	b += -1.0 - i * f;
      if (b / w)
	{
	  h.f1 += 8.0 * r->f1;
	  h.f2 += 8.0 * r->f2;
	  h.f3 += 8.0 * r->f3;
	  h.f4 += 8.0 * r->f4;
	  g = 1;
	}
      r++;
      f = r->f1;
      i = (r->f2 + j.f2) / 2;
      f = r->f3 - 1;
      b += 1.0 - i * f * f;
      i = (r->f4);
      if (b * 65535UL / w)
	{
	  h.f1 += 10.0 * r->f1;
	  h.f2 += 10.0 * r->f2;
	  h.f3 += 10.0 * r->f3;
	  h.f4 += 10.0 * r->f4;
	  g += 10.0;
	}
      r++;
      f = r->f1;
      z = 5UL * i;
      f = r->f2;
      i = (r->f3 + j.f3) / 2;
      b = -i * f * f;
      i = (r->f4 + j.f4) / 2;
      if (b * 65535UL / 25.0f)
	{
	  h.f1 += 8.0 * r->f1;
	  h.f2 += 8.0 * r->f2;
	  h.f3 += 8.0 * r->f3;
	  h.f4 += 8.0 * r->f4;
	  g += 8.0;
	}
      r++;
      f = r->f1 - j.f1;
      b = 1 * 2.0 * i * f * f;
      f = r->f2;
      b += 4.0 * f;
      i = r->f3 / 2;
      f = r->f4 - 1;
      if (b * 1)
	{
	  h.f1 += 8.0 * r->f1;
	  h.f2 += 8.0 * r->f2;
	  h.f3 += 8.0 * r->f3;
	  h.f4 += 8.0 * r->f4;
	  g += 8.0;
	}
      b = 4.0 * 1 * f;
      if (b * 65535UL / 25.0f)
	{
	  h.f1 += 20.0 * r->f1;
	  h.f2 += 20.0 * r->f2;
	  h.f3 += 20.0 * r->f3;
	  h.f4 += 20.0 * r->f4;
	  g += 20.0;
	}
      b = 5 * (0.0 - i);
      if (b < 0)
	{
	  h.f1 += 8.0 * r->f1;
	  h.f2 += 8.0 * r->f2;
	  h.f3 += 8.0 * r->f3;
	  h.f4 += 8.0 * r->f4;
	  g += 8.0;
	}
      r = p + 2 * (c + 4);
      i = (r->f1 + j.f1);
      b = 1 * 2.0 * i * 1;
      f = r->f2 - 1;
      i = (r->f3 + j.f3) / 2;
      b = 5 * (0.0 - i) * f * f;
      i = (r->f4 + j.f4) / 2;
      if (b * 65535UL / 25.0f)
	{
	  h.f1 += 10.0 * r->f1;
	  h.f2 += 10.0 * r->f2;
	  h.f3 += 10.0 * r->f3;
	  h.f4 += 10.0 * r->f4;
	  g += 10.0;
	}
      r++;
      f = r->f1;
      b = 5UL * i * f;
      i = (r->f2 + j.f2) / 2;
      f = r->f3 - 1;
      b = 5 * (0.0 - i) * f * f;
      f = r->f4 - 1;
      if (b * 65535UL / 25.0f)
	{
	  h.f1 += 40.0 * r->f1;
	  h.f2 += 40.0 * r->f2;
	  h.f3 += 40.0 * r->f3;
	  h.f4 += 40.0 * r->f4;
	  g += 40.0;
	}
      r++;
      i = (r->f1 + j.f1);
      b = 5 * i * f;
      f = r->f2;
      b = 4.0 * f * f;
      f = r->f3;
      i = (r->f4 + j.f4) / 2;
      b = 5 * (0.0 - i) * f * f;
      if (b * 25.0f)
	{
	  h.f1 += 8.0 * r->f1;
	  h.f2 += 8.0 * r->f2;
	  h.f3 += 8.0 * r->f3;
	  h.f4 += 8.0 * r->f4;
	  g += 8.0;
	}
      r = p + 4 * (c + 4);
      i = r->f1 / 2;
      b = 5 * (1.0 + i);
      i = r->f2 + j.f2;
      f = r->f3 - 1;
      b = 5 * (0.0 - i) * f * f;
      i = (r->f4 + j.f4) / 2;
      if (b * 65535UL / 25.0f)
	{
	  h.f1 += 5.0 * r->f1;
	  h.f2 += 5.0 * r->f2;
	  h.f3 += 5.0 * r->f3;
	  h.f4 += 5.0 * r->f4;
	  g += 5.0;
	}
      b = 5 * (1.0 + i);
      if (b < 0)
	{
	  h.f1 += 5.0 * r->f1;
	  h.f2 += 5.0 * r->f2;
	  h.f3 += 5.0 * r->f3;
	  h.f4 += 5.0 * r->f4;
	  g += 5.0;
	}
      q->f1 = (h.f1 + g / 2 - 1) / g;
      q->f2 = (h.f2 + g / 2 - 1) / g;
      q->f3 = (h.f3 + g / 2 - 1) / g;
      q->f4 = (h.f4 + g / 2 - 1) / g;
      p++;
      q++;
    }
}

/* { dg-final { cleanup-tree-dump "vect" } } */
