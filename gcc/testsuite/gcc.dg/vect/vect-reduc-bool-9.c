/* PR122365 */
/* { dg-do compile } */

struct TDTI {
  float V[4];
};
struct TDTI4D {
  struct TDTI S[];
};
void bar();
struct TDTI4D nii_readParRec_dti4D;
int nii_readParRec_d_0_0;
void nii_readParRec() {
  for (int i;;) {
    bool v1varies = false, v2varies = false, v3varies = false;
    for (; i < nii_readParRec_d_0_0; i++) {
      if (nii_readParRec_dti4D.S[i].V[1])
        v1varies = true;
      if (nii_readParRec_dti4D.S[i].V[2])
        v2varies = true;
      if (nii_readParRec_dti4D.S[i].V[3])
        v3varies = true;
    }
    if (v1varies || v2varies || v3varies)
      bar();
  }
}
