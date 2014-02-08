/* { dg-do compile } */
/* { dg-options "-O3" } */

double *my_alloc1 (int len) __attribute__((__assume_aligned__ (16)));
double *my_alloc2 (int len) __attribute__((__assume_aligned__ (32, 16)));

void
test1 (int len)
{
  int i;
  double *__restrict o1 = my_alloc1 (len);
  double *__restrict o2 = my_alloc1 (len);
  double *__restrict o3 = my_alloc1 (len);
  double *__restrict i1 = my_alloc1 (len);
  double *__restrict i2 = my_alloc1 (len);
  for (i = 0; i < len; ++i)
    {
      o1[i] = i1[i] * i2[i];
      o2[i] = i1[i] + i2[i];
      o3[i] = i1[i] - i2[i];
    }
}

void
test2 (int len)
{
  int i;
  double *__restrict o1 = my_alloc2 (len);
  double *__restrict o2 = my_alloc2 (len);
  double *__restrict o3 = my_alloc2 (len);
  double *__restrict i1 = my_alloc2 (len);
  double *__restrict i2 = my_alloc2 (len);
  for (i = 0; i < len; ++i)
    {
      o1[i] = i1[i] * i2[i];
      o2[i] = i1[i] + i2[i];
      o3[i] = i1[i] - i2[i];
    }
}
