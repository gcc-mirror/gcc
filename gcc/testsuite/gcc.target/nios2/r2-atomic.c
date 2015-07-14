/* { dg-do assemble } */
/* { dg-options "-O -march=r2" } */

int test_stex (unsigned char* p1, unsigned char* p2)
{
  int a, b, c, d;
  a = __builtin_stex (p1, *p2);
  b = __builtin_stex (p2, 0);
  c = __builtin_stex (p2 + 1, 0x80);
  d = __builtin_stex (p2 + 2, 0x7f);

  return a + b + c + d;
}

int test_stsex (unsigned short* p1, unsigned short* p2)
{
  int a, b, c, d;
  
  a = __builtin_stsex (p1, *p2);
  b = __builtin_stsex (p2, 0);
  c = __builtin_stsex (p2 + 1, 0x8000);
  d = __builtin_stsex (p2 + 2, 0x7fff);

  return a + b + c + d;
}

int test_ldex (unsigned char* p1, unsigned char* p2)
{
  int a, b, c, d;
  
  a = __builtin_ldex (p1);
  b = __builtin_ldex (p2);
  c = __builtin_ldex (p2 + 1);
  d = __builtin_ldex (p2 + 2);

  return a + b + c + d;
}

int test_ldsex (unsigned char* p1, unsigned char* p2)
{
  int a, b, c, d;
  
  a = __builtin_ldsex (p1);
  b = __builtin_ldsex (p2);
  c = __builtin_ldsex (p2 + 1);
  d = __builtin_ldsex (p2 + 2);

  return a + b + c + d;
}
