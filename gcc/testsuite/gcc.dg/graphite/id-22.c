typedef float rvec[3];
float calc_similar_ind (int nind, rvec x[])
{
  int i, j, d;
  float m, tm, xd, rd;
  for(j=0; j<nind; j++) {
    i = x[j][0];
    tm += m;
    for(d=0 ; d<3; d++) {
      xd = x[i][d] - x[i][d];
      rd += m * xd;
    }
  }
    return rd/tm;
}
