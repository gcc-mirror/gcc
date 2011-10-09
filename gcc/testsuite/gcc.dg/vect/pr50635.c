/* { dg-do compile } */

typedef signed long int32_t;
typedef char int8_t;

void f0a(int32_t * result, int32_t * arg1, int8_t * arg2, int32_t temp_3)
{
  int idx;
  for (idx=0;idx<10;idx += 1)
    {
      int32_t temp_4;
      int32_t temp_12;

      temp_4 = (-2 & arg2[idx]) + temp_3;
      temp_12 = -2 * arg2[idx] + temp_4;
      result[idx] = temp_12;
    }
}

/* { dg-final { cleanup-tree-dump "vect" } } */

