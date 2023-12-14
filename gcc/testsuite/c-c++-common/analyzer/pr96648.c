/* { dg-additional-options "-O1" } */

struct vd {
  struct vd *rs;
};

struct fh {
  struct vd cl;
};

struct i3 {
  struct fh *h4;
};

struct fh *
gm (void);

void
j7 (struct vd *);

inline void
mb (struct vd *e7)
{
  j7 (e7->rs);
}

void
po (struct i3 *d2)
{
  struct i3 *s2;

  d2->h4 = gm ();
  mb (&d2->h4->cl);
  s2 = ({ d2 - 1; });
  po (s2);
}
