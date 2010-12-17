int find_sad_16x16(int *intra_mode)
{
  int current_intra_sad_2,best_intra_sad2;
  int M1[16][16],M0[4][4][4][4],M3[4],M4[4][4];
  int i,j,k;
  int ii,jj;
  int up_avail, left_avail, left_up_avail;
  for (i=0;i<17;i++)
    if (left_up_avail)
      {
	for (jj=0;jj<4;jj++)
	  for (ii=0;ii<4;ii++)
	    for (j=0;j<4;j++)
	      for (i=0;i<4;i++)
		{
		  M0[i][ii][2][jj]=M3[0]-M3[1];
		  M0[i][ii][1][jj]=M3[2]+M3[3];
		  current_intra_sad_2 += abs(M0[i][ii][j][jj]);
		}

        if(current_intra_sad_2 < best_intra_sad2)
          best_intra_sad2=current_intra_sad_2;
      }
}
