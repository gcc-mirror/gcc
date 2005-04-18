/* { dg-do run { target mipsisa64*-*-* } } */
/* { dg-mips-options "-mips64 -O2 -mips3d -mhard-float -mgp64" } */

/* Matrix Multiplications */
#include <stdlib.h>
#include <stdio.h>

typedef float v2sf __attribute__((vector_size(8)));

float a[4] = {1.1, 2.2, 3.3, 4.4};
float b[4][4] = {{1, 2, 3, 4},
                 {5, 6, 7, 8},
                 {9, 10, 11, 12},
                 {13, 14, 15, 16}};

float c[4]; /* Result for matrix_multiply1() */
float d[4]; /* Result for matrix_multiply2() */
float e[4]; /* Result for matrix_multiply3() */
float f[4]; /* Result for matrix_multiply4() */

void matrix_multiply1();
void matrix_multiply2();
void matrix_multiply3();
void matrix_multiply4();

int main ()
{
  int i;

  /* Version 1. Use float calculations */
  matrix_multiply1();

  /* Version 2. Use paired-single instructions inside the inner loop*/
  matrix_multiply2();
  for (i = 0; i < 4; i++)
    if (d[i] != c[i])
      abort();

  /* Version 3. Use paired-single instructions and unroll the inner loop */
  matrix_multiply3();
  for (i = 0; i < 4; i++)
    if (e[i] != c[i])
      abort();

  /* Version 4. Use paired-single instructions and unroll all loops */
  matrix_multiply4();
  for (i = 0; i < 4; i++)
    if (f[i] != c[i])
      abort();

  printf ("Test Passes\n");
  exit (0);
}

void matrix_multiply1()
{
  int i, j;

  for (i = 0; i < 4; i++)
   {
     c[i] = 0.0;

     for (j = 0; j < 4; j ++)
       c[i] += a[j] * b[j][i]; 
   }
}

void matrix_multiply2()
{
  int i, j;
  v2sf m1, m2;
  v2sf result, temp;

  for (i = 0; i < 4; i++)
   {
     result = (v2sf) {0.0, 0.0};

     for (j = 0; j < 4; j+=2)
     {
       /* Load two float values into m1 */
       m1 = (v2sf) {a[j], a[j+1]};
       m2 = (v2sf) {b[j][i], b[j+1][i]};

       /* Multiply and add */
       result += m1 * m2;
     }
     
     /* Reduction add at the end */
     temp = __builtin_mips_addr_ps (result, result);
     d[i] = __builtin_mips_cvt_s_pl (temp);
   }
}

void matrix_multiply3()
{
  int i;
  v2sf m1, m2, n1, n2;
  v2sf result, temp;

  m1 = (v2sf) {a[0], a[1]};
  m2 = (v2sf) {a[2], a[3]};

  for (i = 0; i < 4; i++)
   {
     n1 = (v2sf) {b[0][i], b[1][i]};
     n2 = (v2sf) {b[2][i], b[3][i]};

     /* Multiply and add */
     result = m1 * n1 + m2 * n2;
     
     /* Reduction add at the end */
     temp = __builtin_mips_addr_ps (result, result);
     e[i] = __builtin_mips_cvt_s_pl (temp);
   }
}

void matrix_multiply4()
{
  v2sf m1, m2;
  v2sf n1, n2, n3, n4, n5, n6, n7, n8;
  v2sf temp1, temp2, temp3, temp4;
  v2sf result1, result2;

  /* Load a[0] a[1] values into m1
     Load a[2] a[3] values into m2 */
  m1 = (v2sf) {a[0], a[1]};
  m2 = (v2sf) {a[2], a[3]};

  /* Load b[0][0] b[1][0] values into n1
     Load b[2][0] b[3][0] values into n2
     Load b[0][1] b[1][1] values into n3
     Load b[2][1] b[3][1] values into n4
     Load b[0][2] b[1][2] values into n5
     Load b[2][2] b[3][2] values into n6
     Load b[0][3] b[1][3] values into n7
     Load b[2][3] b[3][3] values into n8 */
  n1 = (v2sf) {b[0][0], b[1][0]};
  n2 = (v2sf) {b[2][0], b[3][0]};
  n3 = (v2sf) {b[0][1], b[1][1]};
  n4 = (v2sf) {b[2][1], b[3][1]};
  n5 = (v2sf) {b[0][2], b[1][2]};
  n6 = (v2sf) {b[2][2], b[3][2]};
  n7 = (v2sf) {b[0][3], b[1][3]};
  n8 = (v2sf) {b[2][3], b[3][3]};

  temp1 = m1 * n1 + m2 * n2;
  temp2 = m1 * n3 + m2 * n4;
  temp3 = m1 * n5 + m2 * n6;
  temp4 = m1 * n7 + m2 * n8;

  result1 = __builtin_mips_addr_ps (temp1, temp2);
  result2 = __builtin_mips_addr_ps (temp3, temp4);
  
  f[0] = __builtin_mips_cvt_s_pu (result1);
  f[1] = __builtin_mips_cvt_s_pl (result1);
  f[2] = __builtin_mips_cvt_s_pu (result2);
  f[3] = __builtin_mips_cvt_s_pl (result2);
}
