#include <stdlib.h>

int
main (int argc, char** argv)
{
  const int len = argc == 2 ? atoi(argv[1]) : 4;

  int count;
  int data[64];
  int M1[len][len];
  int M2[len][len];

  for (int i = 0; i < len; i++)
    for (int j = 0 ; j < len ; j++)
      M1[i][j] = M2[i][j] = i*len + j;

  M2[1][0] = M2[0][1];

  /* This writes successively 0 and 1 into data[M2[0][1]].  */
  for (int i = 0; i < len - 1; i++)
    for (int j = 0 ; j < len ; j++)
      if (M1[i+1][j] > M1[i][j]) 
        data[M2[i][j]] = i;

  if (data [M2[0][1]] != 1)
    abort ();

  return 0;
}
