/* { dg-do compile } */
/* { dg-options "-O3 -mzarch -march=z14 -ftree-vectorize -fdump-tree-optimized" } */
/* { dg-do run { target { s390_z14_hw } } } */

/* signed integers */

signed char
__attribute__((noipa, optimize("Ofast")))
reduce_add_char (signed char* p)
{
  signed char sum = 0;
  for (int i = 0; i != 16; i++)
    sum += p[i];
  return sum;
}

short
__attribute__((noipa, optimize("Ofast")))
reduce_add_short (short* p)
{
  short sum = 0;
  for (int i = 0; i != 16; i++)
    sum += p[i];
  return sum;
}

int
__attribute__((noipa, optimize("Ofast")))
reduce_add_int (int* p)
{
  int sum = 0;
  for (int i = 0; i != 16; i++)
    sum += p[i];
  return sum;
}

long
__attribute__((noipa, optimize("Ofast")))
reduce_add_long (long* p)
{
  long sum = 0;
  for (int i = 0; i != 16; i++)
    sum += p[i];
  return sum;
}

/* unsigned integers */

unsigned char
__attribute__((noipa, optimize("Ofast")))
reduce_add_uchar (unsigned char* p)
{
  unsigned char sum = 0;
  for (int i = 0; i != 16; i++)
    sum += p[i];
  return sum;
}

unsigned short
__attribute__((noipa, optimize("Ofast")))
reduce_add_ushort (unsigned short* p)
{
  unsigned short sum = 0;
  for (int i = 0; i != 16; i++)
    sum += p[i];
  return sum;
}

unsigned int
__attribute__((noipa, optimize("Ofast")))
reduce_add_uint (unsigned int* p)
{
  unsigned int sum = 0;
  for (int i = 0; i != 16; i++)
    sum += p[i];
  return sum;
}

unsigned long
__attribute__((noipa, optimize("Ofast")))
reduce_add_ulong (unsigned long* p)
{
  unsigned long sum = 0;
  for (int i = 0; i != 16; i++)
    sum += p[i];
  return sum;
}

/* floating point */

float
__attribute__((noipa, optimize("Ofast")))
reduce_add_float (float* p)
{
  float sum = 0;
  for (int i = 0; i != 16; i++)
    sum += p[i];
  return sum;
}

double
__attribute__((noipa, optimize("Ofast")))
reduce_add_double (double* p)
{
  double sum = 0;
  for (int i = 0; i != 16; i++)
    sum += p[i];
  return sum;
}

int
main()
{
  signed char chararr[] = {-1,-2,-3,-4,-5,-6,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16};
  signed short shortarr[] = {-1,-2,-3,-4,-5,-6,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16};
  signed int intarr[] = {-1,-2,-3,-4,-5,-6,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16};
  signed long longarr[] = {-1,-2,-3,-4,-5,-6,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16};

  unsigned char uchararr[] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
  unsigned short ushortarr[] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
  unsigned int uintarr[] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
  unsigned long ulongarr[] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};

  float floatarr[] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
  double doublearr[] = {-1,-2,-3,-4,-5,-6,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16};

  if (reduce_add_char (chararr) != (-136 & 0xff))
    __builtin_abort();
  if (reduce_add_short (shortarr) != -136)
    __builtin_abort();
  if (reduce_add_int (intarr) != -136)
    __builtin_abort();
  if (reduce_add_long (longarr) != -136)
    __builtin_abort();

  if (reduce_add_uchar (uchararr) != 136)
    __builtin_abort();
  if (reduce_add_ushort (ushortarr) != 136)
    __builtin_abort();
  if (reduce_add_uint (uintarr) != 136)
    __builtin_abort();
  if (reduce_add_ulong (ulongarr) != 136)
    __builtin_abort();

  if (reduce_add_float (floatarr) != 136)
    __builtin_abort();
  if (reduce_add_double (doublearr) != -136)
    __builtin_abort();
  return 0;
}

/* { dg-final { scan-tree-dump-times "\.REDUC_PLUS" 10 "optimized" } } */
