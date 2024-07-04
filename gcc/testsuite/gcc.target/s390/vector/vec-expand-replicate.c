/* Check that the vectorize_vec_perm_const expander correctly deals with
   replication.  Extracted from spec "nab".  */

/* { dg-do compile } */
/* { dg-options "-O3 -mzarch -march=z13 -fvect-cost-model=unlimited" } */

typedef double POINT_T[3];
typedef double MATRIX_T[][4];
typedef struct {
  POINT_T a_pos;
} ATOM_T;
typedef struct {
  ATOM_T *r_atoms;
} RESIDUE_T;
typedef struct strand_t {
  RESIDUE_T *s_residues;
} STRAND_T;
typedef struct strand_t MOLECULE_T;
double xfm_xyz_oxyz4[4];
MOLECULE_T add_he2o3transformmol_mol, add_he2o3transformmol_sp;
RESIDUE_T add_he2o3transformmol_res;
int add_he2o3transformmol_r, add_he2o3transformmol_a, add_he2o3transformmol_i;
ATOM_T *add_he2o3transformmol_ap;
POINT_T add_he2o3transformmol_xyz, add_he2o3transformmol_nxyz;
static void xfm_xyz(POINT_T oxyz, MATRIX_T mat, POINT_T nxyz) {
  int i, j;
  double nxyz4[4];
  for (i = 0; i < 3; i++)
    xfm_xyz_oxyz4[i] = oxyz[i];
  xfm_xyz_oxyz4[3] = 1.0;
  for (i = 0; i < 4; i++) {
    nxyz4[i] = 0.0;
    for (j = 0; j < 4; j++)
      nxyz4[i] += xfm_xyz_oxyz4[j] * mat[j][i];
  }
  for (i = 0; i < 3; i++)
    nxyz[i] = nxyz4[i];
}
void add_he2o3transformmol(MATRIX_T mat, int n) {
  for (add_he2o3transformmol_sp = add_he2o3transformmol_mol;;)
    for (add_he2o3transformmol_r = 0;;) {
      add_he2o3transformmol_res =
          add_he2o3transformmol_sp.s_residues[add_he2o3transformmol_r];
      for (add_he2o3transformmol_a = 0; add_he2o3transformmol_a < n; add_he2o3transformmol_a++) {
        add_he2o3transformmol_ap =
            &add_he2o3transformmol_res.r_atoms[add_he2o3transformmol_a];
        for (add_he2o3transformmol_i = 0; add_he2o3transformmol_i < 3;
             add_he2o3transformmol_i++)
          add_he2o3transformmol_xyz[add_he2o3transformmol_i] =
              add_he2o3transformmol_ap->a_pos[add_he2o3transformmol_i];
        xfm_xyz(add_he2o3transformmol_xyz, mat, add_he2o3transformmol_nxyz);
        for (add_he2o3transformmol_i = 0; add_he2o3transformmol_i < 3;
             add_he2o3transformmol_i++)
          add_he2o3transformmol_ap->a_pos[add_he2o3transformmol_i] =
              add_he2o3transformmol_nxyz[add_he2o3transformmol_i];
      }
    }
}

/* { dg-final { scan-assembler-not "vperm" } } */
