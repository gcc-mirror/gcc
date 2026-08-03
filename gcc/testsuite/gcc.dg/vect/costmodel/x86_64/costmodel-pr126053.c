/* { dg-do compile } */
/* { dg-additional-options "-O3 -fno-signed-zeros -march=x86-64-v3 -fopt-info-vec" } */

typedef double BigReal;

struct Position {
  BigReal x, y, z;
};

struct CompAtom {
  struct Position position;
  float charge;
  short vdwType;
  unsigned char partition;
  unsigned char nonbondedGroupSize;
};

struct Force {
  BigReal x, y, z;
};

struct SimParams {
  BigReal offset_x, offset_y, offset_z;
};

enum { vXX, vXY, vXZ, vYY, vYZ, vZZ, fvXX, fvXY, fvXZ, fvYY, fvYZ, fvZZ };
void calc_pair_energy_fullelect(
    const struct CompAtom *__restrict p_0,
    const struct CompAtom *__restrict p_1,
    const struct SimParams *__restrict params, const int *__restrict pairlist_n,
    const int *__restrict pairlist_m, const int *__restrict npair_n_list,
    const int *__restrict npair_m_list, int i_upper,
    const BigReal *__restrict force_r_vals, struct Force *__restrict f_0,
    struct Force *__restrict f_1, BigReal *__restrict reduction) {
  BigReal virial_xx = 0, virial_xy = 0, virial_xz = 0;
  BigReal virial_yy = 0, virial_yz = 0, virial_zz = 0;
  BigReal fullElectVirial_xx = 0, fullElectVirial_xy = 0,
          fullElectVirial_xz = 0;
  BigReal fullElectVirial_yy = 0, fullElectVirial_yz = 0,
          fullElectVirial_zz = 0;

  int pn = 0, pm = 0;
  for (int i = 0; i < i_upper; ++i) {
    const struct CompAtom *p_i = p_0 + i;
    const BigReal p_i_x = params->offset_x + p_i->position.x;
    const BigReal p_i_y = params->offset_y + p_i->position.y;
    const BigReal p_i_z = params->offset_z + p_i->position.z;

    BigReal f_i_x = 0, f_i_y = 0, f_i_z = 0;

    {
      const int npairi = npair_n_list[i];
      const int *pli = pairlist_n + pn;
      const BigReal *fr = force_r_vals + pn;
      for (int k = 0; k < npairi; ++k) {
        const int j = pli[k];
        const struct CompAtom *p_j = p_1 + j;
        struct Force *f_j = f_1 + j;
        const BigReal p_ij_x = p_i_x - p_j->position.x;
        const BigReal p_ij_y = p_i_y - p_j->position.y;
        const BigReal p_ij_z = p_i_z - p_j->position.z;
        const BigReal force_r = fr[k];
        BigReal tmp_x = force_r * p_ij_x;
        virial_xx += tmp_x * p_ij_x;
        virial_xy += tmp_x * p_ij_y;
        virial_xz += tmp_x * p_ij_z;
        f_i_x += tmp_x;
        f_j->x -= tmp_x; /* { dg-optimized "basic block part vectorized using 16 byte vectors" } */
        BigReal tmp_y = force_r * p_ij_y;
        virial_yy += tmp_y * p_ij_y;
        virial_yz += tmp_y * p_ij_z;
        f_i_y += tmp_y;
        f_j->y -= tmp_y;
        BigReal tmp_z = force_r * p_ij_z;
        virial_zz += tmp_z * p_ij_z;
        f_i_z += tmp_z;
        f_j->z -= tmp_z;
      }
      pn += npairi;
    }
    {
      const int npairi = npair_m_list[i];
      const int *pli = pairlist_m + pm;
      const BigReal *fr = force_r_vals + pm;
      for (int k = 0; k < npairi; ++k) {
        const int j = pli[k];
        const struct CompAtom *p_j = p_1 + j;
        struct Force *f_j = f_1 + j;
        const BigReal p_ij_x = p_i_x - p_j->position.x;
        const BigReal p_ij_y = p_i_y - p_j->position.y;
        const BigReal p_ij_z = p_i_z - p_j->position.z;
        const BigReal force_r = fr[k];
        BigReal tmp_x = force_r * p_ij_x;
        virial_xx += tmp_x * p_ij_x;
        virial_xy += tmp_x * p_ij_y;
        virial_xz += tmp_x * p_ij_z;
        f_i_x += tmp_x;
        f_j->x -= tmp_x; /* { dg-optimized "basic block part vectorized using 16 byte vectors" } */

        BigReal tmp_y = force_r * p_ij_y;
        virial_yy += tmp_y * p_ij_y;
        virial_yz += tmp_y * p_ij_z;
        f_i_y += tmp_y;
        f_j->y -= tmp_y;
        BigReal tmp_z = force_r * p_ij_z;
        virial_zz += tmp_z * p_ij_z;
        f_i_z += tmp_z;
        f_j->z -= tmp_z;
      }
      pm += npairi;
    }

    f_0[i].x += f_i_x; /* { dg-optimized "basic block part vectorized using 16 byte vectors" } */

    f_0[i].y += f_i_y;
    f_0[i].z += f_i_z;
  }

  reduction[vXX] += virial_xx; /* { dg-optimized "basic block part vectorized using 32 byte vectors" } */
  reduction[vXY] += virial_xy;
  reduction[vXZ] += virial_xz;
  reduction[vYY] += virial_yy;
  reduction[vYZ] += virial_yz; /* { dg-optimized "basic block part vectorized using 16 byte vectors" } */
  reduction[vZZ] += virial_zz;
  reduction[fvXX] += fullElectVirial_xx;
  reduction[fvXY] += fullElectVirial_xy;
  reduction[fvXZ] += fullElectVirial_xz;
  reduction[fvYY] += fullElectVirial_yy;
  reduction[fvYZ] += fullElectVirial_yz;
  reduction[fvZZ] += fullElectVirial_zz;
}
