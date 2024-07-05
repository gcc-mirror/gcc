/* { dg-do compile } */
/* { dg-additional-options "-O1" } */

typedef unsigned int v4si __attribute__ ((vector_size (16)));

v4si f1 (v4si a)
{
  v4si zeros = {0,0,0,0};
  return __builtin_shufflevector (a, zeros, 0, 5, 1, 6);
}

typedef unsigned short v8hi __attribute__ ((vector_size (16)));

v8hi f2a (v8hi a)
{
  v8hi zeros = {0,0,0,0,0,0,0,0};
  return __builtin_shufflevector (a, zeros, 0, 9, 1, 10, 2, 11, 3, 12);
}

v8hi f2b (v8hi a)
{
  v8hi zeros = {0,0,0,0,0,0,0,0};
  return __builtin_shufflevector (a, zeros, 0, 5, 1, 6, 2, 7, 3, 8);
}

typedef unsigned char v16qi __attribute__ ((vector_size (16)));

v16qi f3a (v16qi a)
{
  v16qi zeros = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
  return __builtin_shufflevector (a, zeros, 0, 17, 1, 18, 2, 19, 3, 20, 4, 21, 5, 22, 6, 23, 7, 24);
}

v16qi f3b (v16qi a)
{
  v16qi zeros = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
  return __builtin_shufflevector (a, zeros, 0, 5, 1, 6, 2, 7, 3, 8, 4, 9, 5, 10, 6, 11, 7, 12);
}

/* { dg-final { scan-assembler-times {tbl\tv[0-9]+.16b, \{v[0-9]+.16b\}, v[0-9]+.16b} 5 } } */
