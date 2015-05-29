/* { dg-do compile } */

void 
foo (int *data, unsigned len, const int qlp_coeff[], int lp, int residual[])
{
  int i;
  int sum;
  for(i = 0; i < (int)len; i++)
    {     
      sum = 0;   
      sum += qlp_coeff[1] * data[i-2];   
      sum += qlp_coeff[0] * data[i-1];   
      residual[i] = data[i] - (sum >> lp);
    }    
}

