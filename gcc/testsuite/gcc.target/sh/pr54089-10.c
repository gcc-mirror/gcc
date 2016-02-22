/* Check that there are no redundant zero extensions around logical right
   shifts.  */
/* { dg-do compile }  */
/* { dg-options "-O1" }  */
/* { dg-final { scan-assembler-times "extu" 20 } }  */

/* { dg-final { scan-assembler-times "shll" 2 { target { "! sh2a" && has_dyn_shift } } } }  */
/* { dg-final { scan-assembler-times "shll" 3 { target { "! sh2a" && "! has_dyn_shift" } } } }  */
/* { dg-final { scan-assembler-times "movt" 2 { target { ! sh2a } } } }  */

/* { dg-final { scan-assembler-times "bld" 1 { target { sh2a } } } }  */
/* { dg-final { scan-assembler-times "movt" 1 { target { sh2a } } } }  */
/* { dg-final { scan-assembler-times "movrt" 1 { target { sh2a } } } }  */
/* { dg-final { scan-assembler-times "cmp/pz" 1 { target { sh2a } } } }  */

/* { dg-final { scan-assembler-times "shld" 9 { target { has_dyn_shift } } } }  */

void
test_0 (unsigned char* x, unsigned int* y)
{
  y[0] = x[1] >> 1;
}

void
test_1 (unsigned char* x, unsigned int* y)
{
  y[0] = x[1] >> 2;
}

void
test_2 (unsigned char* x, unsigned int* y)
{
  y[0] = x[1] >> 3;
}

void
test_3 (unsigned char* x, unsigned int* y)
{
  y[0] = x[1] >> 4;
}

void
test_4 (unsigned char* x, unsigned int* y)
{
  y[0] = x[1] >> 5;
}

void
test_5 (unsigned char* x, unsigned int* y)
{
  y[0] = x[1] >> 6;
}

void
test_6 (unsigned char* x, unsigned int* y)
{
  /* non-SH2A: shll, movt
     SH2A: bld, movt */
  y[0] = x[1] >> 7;
}


void
test_100 (unsigned short* x, unsigned int* y)
{
  y[0] = x[1] >> 1;
}

void
test_101 (unsigned short* x, unsigned int* y)
{
  y[0] = x[1] >> 2;
}

void
test_102 (unsigned short* x, unsigned int* y)
{
  y[0] = x[1] >> 3;
}

void
test_103 (unsigned short* x, unsigned int* y)
{
  y[0] = x[1] >> 4;
}

void
test_104 (unsigned short* x, unsigned int* y)
{
  y[0] = x[1] >> 5;
}

void
test_105 (unsigned short* x, unsigned int* y)
{
  y[0] = x[1] >> 6;
}

void
test_106 (unsigned short* x, unsigned int* y)
{
  y[0] = x[1] >> 7;
}

void
test_107 (unsigned short* x, unsigned int* y)
{
  y[0] = x[1] >> 8;
}

void
test_108 (unsigned short* x, unsigned int* y)
{
  y[0] = x[1] >> 9;
}

void
test_109 (unsigned short* x, unsigned int* y)
{
  y[0] = x[1] >> 10;
}

void
test_110 (unsigned short* x, unsigned int* y)
{
  y[0] = x[1] >> 11;
}

void
test_111 (unsigned short* x, unsigned int* y)
{
  y[0] = x[1] >> 12;
}

void
test_112 (unsigned short* x, unsigned int* y)
{
  y[0] = x[1] >> 13;
}

void
test_113 (unsigned short* x, unsigned int* y)
{
  y[0] = x[1] >> 14;
}

void
test_114 (unsigned short* x, unsigned int* y)
{
  /* non-SH2A: shll, movt
     SH2A: cmp/pz, movrt */
  y[0] = x[1] >> 15;
}
