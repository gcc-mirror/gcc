/* { dg-do compile } */

extern int xdo_rb_ctr_row( int *pos_code);

int xgp_ahd_interpolate (int tile)
{
 int p[4];

 switch (tile) {
 default:
 case 0:
 case 1:
  p[0] = 0; p[1] = 1; p[2] = 2; p[3] = 3;
  break;
 case 2:
 case 3:
  p[0] = 1; p[1] = 0; p[2] = 3; p[3] = 2;
  break;
 case 4:
 case 5:
  p[0] = 3; p[1] = 2; p[2] = 1; p[3] = 0;
  break;
 case 6:
 case 7:
  p[0] = 2; p[1] = 3; p[2] = 0; p[3] = 1;
  break;
 }

 xdo_rb_ctr_row(p);
 xdo_rb_ctr_row(p);
 return 0;
}

/* { dg-final { cleanup-tree-dump "vect" } } */

