/* { dg-do compile } */

void foo (int *data, unsigned len, const int qlp_coeff[],
	  unsigned order, int lp, int residual[])
{
  int i;
  int sum;
  if(order == 2)
    for(i = 0; i < (int)len; i++)
      {
	sum = 0;
	sum += qlp_coeff[1] * data[i-2]; 
	sum += qlp_coeff[0] * data[i-1];
	residual[i] = data[i] - (sum >> lp);
      }
  else
    for(i = 0; i < (int)len; i++)  
      residual[i] = data[i] - ((qlp_coeff[0] * data[i-1]) >> lp);
}

/* { dg-final { cleanup-tree-dump "vect" } } */
