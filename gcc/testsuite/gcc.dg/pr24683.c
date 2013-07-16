/* { dg-do compile { target fpic } } */
/* { dg-options "-O2 -fPIC" } */
const int *block;
void final(unsigned int j)
{
  unsigned int i;
  const unsigned char *data = (const unsigned char *)"\0";
  for (i = 0; i < 8; i++)
    for (; j + 63 < 1; j += 64)
      block = (const int *) &data[j];
}
