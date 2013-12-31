/* { dg-do compile } */

void test_stbio (unsigned char* p1, unsigned char* p2)
{
  __builtin_stbio (p1, *p2);
  __builtin_stbio (p2, 0);
  __builtin_stbio (p2 + 1, 0x80);
  __builtin_stbio (p2 + 2, 0x7f);
}

void test_sthio (unsigned short* p1, unsigned short* p2)
{
  __builtin_sthio (p1, *p2);
  __builtin_sthio (p2, 0);
  __builtin_sthio (p2 + 1, 0x8000);
  __builtin_sthio (p2 + 2, 0x7fff);
}

void test_stwio (unsigned int* p1, unsigned int* p2)
{
  __builtin_stwio (p1, *p2);
  __builtin_stwio (p2, 0);
  __builtin_stwio (p2 + 1, 0x80000000);
  __builtin_stwio (p2 + 2, 0x7fffffff);
}
