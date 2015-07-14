/* { dg-do assemble } */
/* { dg-options "-O" } */

void test_ldbio (unsigned char* p1, unsigned char* p2)
{
  __builtin_ldbio (p1);
  __builtin_ldbio (p2);
  __builtin_ldbio (p2 + 1);
  __builtin_ldbio (p2 + 2);
  __builtin_ldbio (p2 + 2047);
  __builtin_ldbio (p2 + 2048);
}

void test_ldbuio (unsigned char* p1, unsigned char* p2)
{
  __builtin_ldbuio (p1);
  __builtin_ldbuio (p2);
  __builtin_ldbuio (p2 + 1);
  __builtin_ldbuio (p2 + 2);
  __builtin_ldbuio (p2 + 2047);
  __builtin_ldbuio (p2 + 2048);
}

void test_ldhio (unsigned short* p1, unsigned short* p2)
{
  __builtin_ldhio (p1);
  __builtin_ldhio (p2);
  __builtin_ldhio (p2 + 1);
  __builtin_ldhio (p2 + 2);
  __builtin_ldhio (p2 + 1023);
  __builtin_ldhio (p2 + 1024);
}

void test_ldhuio (unsigned short* p1, unsigned short* p2)
{
  __builtin_ldhuio (p1);
  __builtin_ldhuio (p2);
  __builtin_ldhuio (p2 + 1);
  __builtin_ldhuio (p2 + 2);
  __builtin_ldhuio (p2 + 1023);
  __builtin_ldhuio (p2 + 1024);
}

void test_ldwio (unsigned int* p1, unsigned int* p2)
{
  __builtin_ldwio (p1);
  __builtin_ldwio (p2);
  __builtin_ldwio (p2 + 1);
  __builtin_ldwio (p2 + 2);
  __builtin_ldwio (p2 + 511);
  __builtin_ldwio (p2 + 512);
}
