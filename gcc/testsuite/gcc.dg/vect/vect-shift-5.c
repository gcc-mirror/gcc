/* { dg-do compile } */
/* { dg-require-effective-target vect_shift } */
/* { dg-require-effective-target vect_int } */

typedef unsigned int uint32_t;
typedef short unsigned int uint16_t;

int foo (uint32_t arr[4][4])
{
  int sum = 0;
  for(int i = 0; i < 4; i++)
    {
      sum += ((arr[0][i] >> 10) * 20) + ((arr[1][i] >> 11) & 53)
	     + ((arr[2][i] >> 12) * 7)  + ((arr[3][i] >> 13) ^ 43);
    }
    return (((uint16_t)sum) + ((uint32_t)sum >> 16)) >> 1;
}

/* { dg-final { scan-tree-dump {vectorizable_shift ===[\n\r][^\n]*prologue_cost = 0} "vect" } } */
