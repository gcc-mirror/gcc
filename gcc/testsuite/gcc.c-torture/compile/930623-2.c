extern unsigned short statesCount;
extern short cn_cadrstates[38][37];
extern short coutstate[38][37][5];
extern short ccostate[38][37][5];
extern short cdel_E[38][37][5];
extern short cn[38][37][5][3];
extern short cd[38][37][5][3];

extern short c[4][37];

extern double vrandNext[64];

f (unsigned long long maddrOutState[37][8 * 8],
   int maddrIndices[8 * 8],
   int delta_E[8 * 8],
   int after_x_hash[8 * 8],
   int after_y_hash[8 * 8],
   int after_z_hash[8 * 8],
   int nActivePlane)
{
  int class[8 * 8];
  int ccoeff[8 * 8];
  int nx[8 * 8];
  int ny[8 * 8];
  int nz[8 * 8];
  int phat[8 * 8];
  int i;
  int inState;

  for (inState = 0; inState < statesCount; inState++) {
    long long Nf[8 * 8];
    long long Ns[8 * 8];
    long long Nb[8 * 8];
    int bbState[8 * 8];
    int not_flag_p[8 * 8];
    int j;
    int max_cadrstates = 0;

    for (i = 0; i < nActivePlane; i++) {
      int in = maddrIndices[i];
      int k = cn_cadrstates[class[in]][inState];

      if (k > max_cadrstates)
	max_cadrstates = k;
    }

    for (j = 0; j < max_cadrstates; j++) {
      int coState[8 * 8];
      int N_co[8 * 8];
      for (i = 0; i < nActivePlane; i++) {
	int in = maddrIndices[i];
	int k = cn_cadrstates[class[in]][inState];

	if (j < k-1) {
	  long long numer = (nx[in] * cn[class[in]][inState][j][0] +
			     ny[in] * cn[class[in]][inState][j][1] +
			     nz[in] * cn[class[in]][inState][j][2]);
	  long long denom = (nx[in] * cd[class[in]][inState][j][0] +
			     ny[in] * cd[class[in]][inState][j][1] +
			     nz[in] * cd[class[in]][inState][j][2]);
	  long long Nj = ((denom == 0) ? 0 : (((((long long)(const64(0,0x10000)) * numer * Ns[in]) / denom) + (long long)(((unsigned) vrandNext[in]) & 0xffff)) >> 16));
	  int outState = coutstate[class[in]][inState][j];
	  int this_coState = ccostate[class[in]][inState][j];
	  int del_E = cdel_E[class[in]][inState][j];
	  int old_Ns = Ns[in];

	  maddrOutState[outState][in] += Nj;
	  Ns[in] -= Nj;
	  delta_E[in] += Nj * del_E;
	  if (not_flag_p[in]) {
	    after_x_hash[in] += Nj * c[0][outState];
	    after_y_hash[in] += Nj * c[1][outState];
	    after_z_hash[in] += Nj * c[2][outState];
	  }
	  coState[in] = this_coState;
	  N_co[in] = Nj;
	}
	else if (j == k-1) {
	  long long Nj = Ns[in];
	  int outState = coutstate[class[in]][inState][j];
	  int this_coState = ccostate[class[in]][inState][j];
	  int del_E = cdel_E[class[in]][inState][j];
	  maddrOutState[outState][in] += Nj;
	  delta_E[in] += Nj * del_E;
	  coState[in] = this_coState;
	  N_co[in] = Nj;
	}
      }
    }
  }
}
