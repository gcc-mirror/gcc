/* { dg-do assemble } */
/* { dg-skip-if "ptxas times out" { nvptx-*-* } { "-O1" } { "" } } */

foo ()
{
  int r0[8186 ];
  int r1[2 ];
  int r2[2 ];
  int bitm0[2 ];
  int bitm1[2 ];
  int bitm2[2 ];

  int i,j,k,m,n,m_max;
  int f;
  double fm,ft;

  while (1) {

    if (m%4 == 2)
      ++m;

    if (m_max != 0 && m > m_max)
      break;

    fm=m;

    r0[k=1]=0;
    bitm0[0] = 0;

      while ( n%f == 0 ) {
	while ( (ft != 0) && (ft < fm )) {
	  bitm1[i] = 0;
	  r1[i]=0;
          }

	while ( r0[i] != 0 && r1[i] != 0 ) {
	  if ( r0[i] < r1[i] ) {
	    bitm2[k] = bitm0[i];
	    r2[k++]=0;
	  }
	  else if ( r0[i] > r1[j] ) {
	    bitm2[k] = bitm1[j];
	    r2[k++]=r1[j++];
	  }
	  else {
	    bitm1[k] = bitm0[i];
	    r2[k++]=r0[i++];
	  }
	}
      }
  }
}
