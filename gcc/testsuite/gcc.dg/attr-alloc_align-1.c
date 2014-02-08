/* { dg-do compile } */
/* { dg-options "-O3" } */

double *my_alloc1 (int len, int align) __attribute__((__alloc_align__ (2)));
double *my_alloc2 (int align, int len) __attribute__((alloc_align (1)));

void
test1 (int len, int align)
{
  int i;
  double *__restrict o1 = my_alloc1 (len, 32);
  double *__restrict o2 = my_alloc1 (len, 32);
  double *__restrict o3 = my_alloc1 (len, 32);
  double *__restrict i1 = my_alloc1 (len, 32);
  double *__restrict i2 = my_alloc1 (len, align);
  for (i = 0; i < len; ++i)
    {
      o1[i] = i1[i] * i2[i];
      o2[i] = i1[i] + i2[i];
      o3[i] = i1[i] - i2[i];
    }
}

void
test2 (int len, int align)
{
  int i;
  double *__restrict o1 = my_alloc2 (32, len);
  double *__restrict o2 = my_alloc2 (32, len);
  double *__restrict o3 = my_alloc2 (32, len);
  double *__restrict i1 = my_alloc2 (32, len);
  double *__restrict i2 = my_alloc2 (align, len);
  for (i = 0; i < len; ++i)
    {
      o1[i] = i1[i] * i2[i];
      o2[i] = i1[i] + i2[i];
      o3[i] = i1[i] - i2[i];
    }
}
