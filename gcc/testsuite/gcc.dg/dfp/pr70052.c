/* { dg-do compile } */
/* { dg-options "-O1" } */

typedef struct
{
  _Decimal128 td0;
  _Decimal128 td1;
} TDx2_t;


TDx2_t
D256_add_finite (void)
{
  _Decimal128 z, zz;
  TDx2_t result = {0.DL, 0.DL};

  if (zz == 0.DL)
  {
    result.td0 = z;
    return result;
  }

  return result;
}
