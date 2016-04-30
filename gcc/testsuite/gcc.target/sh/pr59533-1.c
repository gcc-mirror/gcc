/* Check that the cmp/pz instruction is generated as expected.  */
/* { dg-do compile }  */
/* { dg-options "-O1" } */

/* { dg-final { scan-assembler-times "shll" 1 } }  */
/* { dg-final { scan-assembler-times "movt" 5 } }  */
/* { dg-final { scan-assembler-times "rotcl" 1 } }  */
/* { dg-final { scan-assembler-times "and" 3 } }  */
/* { dg-final { scan-assembler-times "extu.b" 5 } }  */

/* { dg-final { scan-assembler-times "cmp/pz" 27 { target { ! sh2a } } } }  */
/* { dg-final { scan-assembler-times "addc" 4 { target { ! sh2a } } } }  */
/* { dg-final { scan-assembler-times "subc" 16 { target { ! sh2a } } } }  */

/* { dg-final { scan-assembler-times "cmp/pz" 25 { target { sh2a } } } }  */
/* { dg-final { scan-assembler-times "addc" 6 { target { sh2a } } } }  */
/* { dg-final { scan-assembler-times "subc" 14 { target { sh2a } } } }  */
/* { dg-final { scan-assembler-times "bld" 2 { target { sh2a } } } }  */

int
test_00 (unsigned char* a)
{
  /* 1x cmp/pz, 1x movt  */
  return a[0] < 128;
}

int
test_01 (unsigned char* a)
{
  /* 1x cmp/pz, 1x addc  */
  return a[0] + (a[0] < 128);
}

int
test_02 (unsigned char* a)
{
  /* 1x cmp/pz, 1x addc  */
  return a[0] + ((a[0] & 0x80) == 0);
}

int
test_03 (unsigned char* a)
{
  /* 1x cmp/pz, 1x subc
     SH2A: 1x bld, 1x addc  */
  return a[0] + (a[0] > 127);
}

int
test_04 (unsigned char* a)
{
  /* 1x cmp/pz, 1x subc
     SH2A: 1x bld, 1x addc  */
  return a[0] + ((a[0] & 0x80) != 0);
}

int
test_05 (unsigned char* a, int b, int c)
{
  /* 1x cmp/pz  */
  if (a[0] < 128)
    return c;
  else
    return b + 50;
}

unsigned int
test_06 (unsigned int a)
{
  /* 1x cmp/pz, 1x movt  */
  return ~a >> 31;
}

int
test_07 (unsigned short* a)
{
  /* 1x cmp/pz  */
  return a[0] < 32768;
}

int
test_08 (unsigned short* a)
{
  /* 1x cmp/pz, 1x addc  */
  return a[0] + (a[0] < 32768);
}

unsigned int
test_09 (unsigned int a)
{
  /* 1x cmp/pz, 1x movt  */
  return (a >> 31) ^ 1;
}

unsigned int
test_10 (unsigned int a, unsigned int b)
{
  /* 1x cmp/pz, 1x rotcl  */
  return (a << 1) | ((a >> 31) ^ 1);
}

unsigned int
test_11 (int x)
{
  /* 1x cmp/pz, 1x subc  */
  return ~(x >> 31);
}

unsigned int
test_12 (int x)
{
  /* 1x cmp/pz, 1x subc  */
  return 0xFFFFFFFF - (x >> 31);
}

unsigned int
test_13 (int x)
{
  /* 1x cmp/pz, 1x subc, 1x add  */
  return ~(x >> 31) << 1;
}

unsigned int
test_14 (int x)
{
  /* 1x cmp/pz, 1x subc  */
  return ~(x >> 31) >> 1;
}

unsigned int
test_15 (int x)
{
  /* 1x cmp/pz, 1x subc  */
  return ~(x >> 31) >> 31;
}

unsigned int
test_16 (int x)
{
  /* 1x cmp/pz, 1x subc, 1x and  */
  return ~(x >> 31) & 0xFF000000;
}

unsigned int
test_17 (int x)
{
  /* 1x cmp/pz, 1x subc, 1x and  */
  return ~(x >> 31) & 0x00FF0000;
}

unsigned int
test_18 (int x)
{
  /* 1x cmp/pz, 1x subc, 1x and  */
  return ~(x >> 31) & 0x0000FF00;
}

unsigned int
test_19 (int x)
{
  /* 1x cmp/pz, 1x subc, 1x extu.b  */
  return ~(x >> 31) & 0x000000FF;
}

unsigned int
test_20 (int x, unsigned int y, unsigned int z)
{
  /* 1x shll  */
  return ~(x >> 31) ? y : z;
}

int
test_21 (int x)
{
  /* 1x cmp/pz, 1x subc  */
  return x >= 0 ? 0xFFFFFFFF : 0;
}

int
test_22 (int x)
{
  /* 1x cmp/pz, 1x movt  */
  return (x >> 31) + 1;
}

int
test_23 (int x)
{
  /* 1x cmp/pz, 1x subc */
  return x < 0 ? x + 1 : x;
}

unsigned int
test_24 (unsigned int x)
{
  /* 1x cmp/pz, 1x subc */
  return x & 0x80000000 ? x + 1 : x;
}

unsigned int
test_25 (unsigned int x)
{
  /* 1x cmp/pz, 1x subc */
  return x >> 31 ? x + 1 : x;
}

int
test_26 (int x)
{
  /* 1x cmp/pz, 1x subc  */
  return x >> 31 ? x + 1 : x;
}

int
test_27 (int x, int y, int z)
{
  /* 1x cmp/pz, 1x addc  */
  return 1 - ((x >> 4) < 0) + z;
}
