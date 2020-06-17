/* { dg-additional-options "-Wno-incompatible-pointer-types -Wno-analyzer-too-complex" } */
/* TODO: ideally we shouldn't have -Wno-analyzer-too-complex above; it
   appears to be needed due to the recursion.  */

struct tz {
  int qc;
};

struct wp {
  struct tz *p2;
} *ov;

struct dz {
  struct wp *r5;
};

void
za (void);

void
h5 (struct dz *);

int
e7 (struct wp *f2)
{
  return f2 == ov;
}

void
wr (struct wp *sw)
{
  if (sw != 0)
    za ();
}

void
m6 (const struct dz *gq)
{
  wr (gq->r5);

  asm ("" : "+m" (gq));

  if (0)
    {
      asm ("" : "+m" (gq->r5->p2->qc));
      asm ("" : "+m" (gq->r5->p2->qc));
    }

  asm ("" : "+m" (gq->r5->p2->qc));

  if (e7 (gq->r5))
    za ();
}

void
ts (struct dz *cx)
{
  struct dz nt;

  if (nt.r5)
    {
      m6 (cx);
      h5 (cx);
      ts (&cx);
    }
}
