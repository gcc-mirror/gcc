/* Verify that calls to strcpy to write to an element of an array of pointers
   are not diagnosed (due to mistakenly using the size of the array as that
   of the destination).
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

typedef char A1[1];
typedef char A2[2];
typedef char A3[3];
typedef char A4[4];
typedef char A5[5];
typedef char A6[6];
typedef char A7[7];
typedef char A8[8];
typedef char A9[9];
typedef char A10[10];

A1* pa1[3];
A2* pa2[3];
A3* pa3[3];
A4* pa4[3];
A5* pa5[3];
A6* pa6[3];
A7* pa7[3];
A8* pa8[3];
A9* pa9[3];
A10* pa10[3];

void nowarn_a1_1 (int i)
{
  __builtin_strcpy (*pa1[0], "");
  __builtin_strcpy (*pa1[1], "");
  __builtin_strcpy (*pa1[i], "");
}

void nowarn_a2_2 (int i)
{
  __builtin_strcpy (*pa2[0], "1");
  __builtin_strcpy (*pa2[1], "2");
  __builtin_strcpy (*pa2[i], "3");
}

void nowarn_a3_3 (int i)
{
  __builtin_strcpy (*pa3[0], "12");
  __builtin_strcpy (*pa3[1], "23");
  __builtin_strcpy (*pa3[i], "34");
}

void nowarn_a4_4 (int i)
{
  __builtin_strcpy (*pa4[0], "123");
  __builtin_strcpy (*pa4[1], "234");
  __builtin_strcpy (*pa4[i], "345");
}

void nowarn_a5_5 (int i)
{
  __builtin_strcpy (*pa5[0], "1234");
  __builtin_strcpy (*pa5[1], "2345");
  __builtin_strcpy (*pa5[i], "3456");
}

void nowarn_a6_6 (int i)
{
  __builtin_strcpy (*pa6[0], "12345");
  __builtin_strcpy (*pa6[1], "23456");
  __builtin_strcpy (*pa6[1], "34567");
}

void nowarn_a10_10 (int i)
{
  __builtin_strcpy (*pa10[0], "0123456789");
  __builtin_strcpy (*pa10[1], "1234567890");
  __builtin_strcpy (*pa10[i], "2345678909");
}
