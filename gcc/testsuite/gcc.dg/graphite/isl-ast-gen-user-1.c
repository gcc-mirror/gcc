static const int N = 12;
int nSlip;

int main ()
{
  int i, j, k, fdot = 0;
  int a[N][N];

  for ( i = 1; i < nSlip; i++)
    {
      for ( j = i+1; j < nSlip; j++)
        {
          for ( k = 0; k < i; k++)
            fdot += a[i][k] * a[k][j];
          a[i][j] = a[i][j] - fdot;
        }
   }

  return 0;
}



