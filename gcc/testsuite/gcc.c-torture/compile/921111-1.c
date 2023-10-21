/* { dg-additional-options "-std=gnu89" } */

int ps;
struct vp {
  int wa;
};
typedef struct vp *vpt;
typedef struct vc {
  int o;
  vpt py[8];
} *vct;
struct n {
  int a;
};
struct nh {
  int x;
};
typedef struct np *npt;
struct np {
  vct d;
  int di;
};
struct nh xhp;
struct n np[3];

f(dp)
     npt dp;
{
  vpt *py;
  int a, l, o = 0;
  a = dp->d->o;
  if (dp->di < 0)
    l = ps;

  if ((int)o & 3)
    g();

  xhp.x = a;
  py = &dp->d->py[dp->di];
  if (o + l > ps)
    np[2].a = (int)(py[1])->wa;
}
