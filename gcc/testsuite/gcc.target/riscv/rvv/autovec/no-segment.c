/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -mrvv-vector-bits=scalable -O3 -mno-autovec-segment" } */

enum e { c, d };
enum g { f };

struct h
{
  float x, w;
};

struct k
{
  short z, y, i, j;
};

long r;
struct h m, p;
struct k *q;

short
l (float s)
{
  if (s <= 0.0f)
    return 0;

  if (s >= 5)
    return 5;

  return s;
}

struct n
{
  enum g colorspace;
};

struct n o (struct k *s, struct h *t)
{
  t->w = s->z;
}

void
ClutImageChannel (struct n *s, enum e t)
{

  while (s)
    for (; r; r++)
      {
	o (q, &p);

	if (t & d)
	  q->y = (&m + q->y)->x;

	if (t)
	  q->z = l ((&m + q->z)->w);

	if (s->colorspace)
	  q++;
      }
}
