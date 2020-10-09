/* PR middle-end/96384 - bogus -Wstringop-overflow= storing into
   multidimensional array with index in range
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

#define SHRT_MAX   __SHRT_MAX__
#define SHRT_MIN   (-SHRT_MAX - 1)
#define INT_MAX    __INT_MAX__
#define INT_MIN    (-INT_MAX - 1)
#define LONG_MAX   __LONG_MAX__
#define LONG_MIN   (-LONG_MAX - 1)

#define USHRT_MAX  (SHRT_MAX * 2 + 1)
#define UINT_MAX   ~0U
#define ULONG_MAX  ~0LU

char ca3_5_7[3][5][7];

void nowarn_ca_3_5_ssi (short i)
{
  if (i > SHRT_MAX - 1)
    i = SHRT_MAX - 1;

  ca3_5_7[i][0][0] = __LINE__;
  ca3_5_7[i][0][1] = __LINE__;
  ca3_5_7[i][0][2] = __LINE__;
  ca3_5_7[i][0][3] = __LINE__;
  ca3_5_7[i][0][4] = __LINE__;
  ca3_5_7[i][0][5] = __LINE__;
  ca3_5_7[i][0][6] = __LINE__;

  ca3_5_7[i][1][0] = __LINE__;
  ca3_5_7[i][1][1] = __LINE__;
  ca3_5_7[i][1][2] = __LINE__;
  ca3_5_7[i][1][3] = __LINE__;
  ca3_5_7[i][1][4] = __LINE__;
  ca3_5_7[i][1][5] = __LINE__;
  ca3_5_7[i][1][6] = __LINE__;

  ca3_5_7[i][2][0] = __LINE__;
  ca3_5_7[i][2][1] = __LINE__;
  ca3_5_7[i][2][2] = __LINE__;
  ca3_5_7[i][2][3] = __LINE__;
  ca3_5_7[i][2][4] = __LINE__;
  ca3_5_7[i][2][5] = __LINE__;
  ca3_5_7[i][2][6] = __LINE__;

  ca3_5_7[i][3][0] = __LINE__;
  ca3_5_7[i][3][1] = __LINE__;
  ca3_5_7[i][3][2] = __LINE__;
  ca3_5_7[i][3][3] = __LINE__;
  ca3_5_7[i][3][4] = __LINE__;
  ca3_5_7[i][3][5] = __LINE__;
  ca3_5_7[i][3][6] = __LINE__;

  ca3_5_7[i][4][0] = __LINE__;
  ca3_5_7[i][4][1] = __LINE__;
  ca3_5_7[i][4][2] = __LINE__;
  ca3_5_7[i][4][3] = __LINE__;
  ca3_5_7[i][4][4] = __LINE__;
  ca3_5_7[i][4][5] = __LINE__;
  ca3_5_7[i][4][6] = __LINE__;

  ca3_5_7[1][i][5] = __LINE__;
  ca3_5_7[2][3][i] = __LINE__;
}

void nowarn_ca_3_5_usi (unsigned short i)
{
  if (i > USHRT_MAX - 1)
    i = USHRT_MAX - 1;

  ca3_5_7[i][3][5] = __LINE__;
  ca3_5_7[1][i][5] = __LINE__;
  ca3_5_7[2][3][i] = __LINE__;
}

void nowarn_ca_3_5_si (int i)
{
  if (i > INT_MAX - 1)
    i = INT_MAX - 1;

  ca3_5_7[i][3][5] = __LINE__;
  ca3_5_7[1][i][5] = __LINE__;
  ca3_5_7[2][3][i] = __LINE__;
}

void nowarn_ca_3_5_ui (unsigned i)
{
  if (i > UINT_MAX - 1)
    i = UINT_MAX - 1;

  ca3_5_7[i][3][5] = __LINE__;
  ca3_5_7[1][i][5] = __LINE__;
  ca3_5_7[2][3][i] = __LINE__;
}

void nowarn_ca_3_5_li (long i)
{
  if (i > LONG_MAX - 1)
    i = LONG_MAX - 1;

  ca3_5_7[i][3][5] = __LINE__;
  ca3_5_7[1][i][5] = __LINE__;
  ca3_5_7[2][3][i] = __LINE__;
}

void nowarn_ca_3_5_uli (unsigned long i)
{
  if (i > ULONG_MAX - 1)
    i = ULONG_MAX - 1;

  ca3_5_7[i][3][5] = __LINE__;
  ca3_5_7[1][i][5] = __LINE__;
  ca3_5_7[2][3][i] = __LINE__;
}
