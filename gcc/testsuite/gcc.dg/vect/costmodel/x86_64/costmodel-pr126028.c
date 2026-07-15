/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-slp1 -fopt-info-vec" } */

typedef double BigReal;

struct Position { BigReal x, y, z; };

struct CompAtom {
    struct Position position;
    float    charge;
    short    vdwType;
    unsigned char partition;
    unsigned char nonbondedGroupSize;
};

void cull_goodgroups(const struct CompAtom * __restrict p_1,
		     const int      * __restrict glist,
		     int gu, 
		     BigReal p_i_x, BigReal p_i_y, BigReal p_i_z,
		     BigReal groupplcutoff2,
		     int * __restrict goodglist)
{
  int hu = 0;
  int g = 0;
  int jprev0 = glist[0];
  int jprev1 = glist[1];
  int j0, j1; 

  BigReal pj_x_0, pj_x_1, pj_y_0, pj_y_1, pj_z_0, pj_z_1;
  BigReal t_0, t_1, r2_0, r2_1;

  pj_x_0 = p_1[jprev0].position.x;  pj_x_1 = p_1[jprev1].position.x;
  pj_y_0 = p_1[jprev0].position.y;  pj_y_1 = p_1[jprev1].position.y;
  pj_z_0 = p_1[jprev0].position.z;  pj_z_1 = p_1[jprev1].position.z;
  g += 2;

  for ( ; g < gu - 2; g += 2 ) { 
      j0 = jprev0;
      j1 = jprev1;

      t_0 = p_i_x - pj_x_0;  t_1 = p_i_x - pj_x_1;
      r2_0 = t_0 * t_0;      r2_1 = t_1 * t_1;
      t_0 = p_i_y - pj_y_0;  t_1 = p_i_y - pj_y_1;
      r2_0 += t_0 * t_0;     r2_1 += t_1 * t_1;
      t_0 = p_i_z - pj_z_0;  t_1 = p_i_z - pj_z_1;
      r2_0 += t_0 * t_0;     r2_1 += t_1 * t_1;

      jprev0 = glist[g];
      jprev1 = glist[g + 1]; 
      pj_x_0 = p_1[jprev0].position.x;  pj_x_1 = p_1[jprev1].position.x;
      pj_y_0 = p_1[jprev0].position.y;  pj_y_1 = p_1[jprev1].position.y;
      pj_z_0 = p_1[jprev0].position.z;  pj_z_1 = p_1[jprev1].position.z;

      bool test0 = (r2_0 < groupplcutoff2);
      bool test1 = (r2_1 < groupplcutoff2);

      goodglist[hu]         = j0;
      goodglist[hu + test0] = j1;
      hu += test0 + test1; /* { dg-optimized "basic block part vectorized using 8 byte vectors" } */
  }
}

/* { dg-final { scan-tree-dump-times " = vect_t\[^ \]* \\\* vect_t" 3 "slp1" } } */
