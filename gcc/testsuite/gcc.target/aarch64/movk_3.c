/* { dg-do compile } */
/* { dg-options "-O2 --save-temps" } */


/* 2 MOV */
void f16 (long *p)
{
  p[0] = 0x1234;
  p[2] = 0x1235;
}

/* MOV, MOVK and ADD */
void f32_1 (long *p)
{
  p[0] = 0x12345678;
  p[2] = 0x12345678 + 0xfff;
}

/* 2 MOV, 2 MOVK */
void f32_2 (long *p)
{
  p[0] = 0x12345678;
  p[2] = 0x12345678 + 0x555555;
}

/* MOV, MOVK and ADD */
void f32_3 (long *p)
{
  p[0] = 0x12345678;
  p[2] = 0x12345678 + 0x999000;
}

/* MOV, 2 MOVK and ADD */
void f48_1 (long *p)
{
  p[0] = 0x123456789abc;
  p[2] = 0x123456789abc + 0xfff;
}

/* MOV, 2 MOVK and 2 ADD */
void f48_2 (long *p)
{
  p[0] = 0x123456789abc;
  p[2] = 0x123456789abc + 0x666666;
}

/* 2 MOV, 4 MOVK */
void f48_3 (long *p)
{
  p[0] = 0x123456789abc;
  p[2] = 0x123456789abc + 0x1666666;
}

/* { dg-final { scan-assembler-times "mov\tx\[0-9\]+, \[0-9\]+" 10 } } */
/* { dg-final { scan-assembler-times "movk\tx\[0-9\]+, 0x\[0-9a-f\]+" 12 } } */
/* { dg-final { scan-assembler-times "add\tx\[0-9\]+, x\[0-9\]+, \[0-9\]+" 5 } } */
