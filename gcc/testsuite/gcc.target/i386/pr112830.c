/* { dg-do compile } */
/* { dg-options "" } */

void
foo (int n, __seg_fs void *p, __seg_gs void *q)
{
  typedef struct { char t[n]; } T;
  *(__seg_fs T *)p = *(__seg_gs T *)q;
}
