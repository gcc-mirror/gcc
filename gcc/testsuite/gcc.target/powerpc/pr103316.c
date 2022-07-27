/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-require-effective-target int128 } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

vector bool __int128
test_eq (vector signed __int128 a, vector signed __int128 b)
{
  return a == b;
}

vector bool __int128
test_ne (vector signed __int128 a, vector signed __int128 b)
{
  return a != b;
}

vector bool __int128
test_gt (vector signed __int128 a, vector signed __int128 b)
{
  return a > b;
}

vector bool __int128
test_ge (vector signed __int128 a, vector signed __int128 b)
{
  return a >= b;
}

vector bool __int128
test_lt (vector signed __int128 a, vector signed __int128 b)
{
  return a < b;
}

vector bool __int128
test_le (vector signed __int128 a, vector signed __int128 b)
{
  return a <= b;
}

vector bool __int128
testu_eq (vector unsigned __int128 a, vector unsigned __int128 b)
{
  return a == b;
}

vector bool __int128
testu_ne (vector unsigned __int128 a, vector unsigned __int128 b)
{
  return a != b;
}

vector bool __int128
testu_gt (vector unsigned __int128 a, vector unsigned __int128 b)
{
  return a > b;
}

vector bool __int128
testu_ge (vector unsigned __int128 a, vector unsigned __int128 b)
{
  return a >= b;
}

vector bool __int128
testu_lt (vector unsigned __int128 a, vector unsigned __int128 b)
{
  return a < b;
}

vector bool __int128
testu_le (vector unsigned __int128 a, vector unsigned __int128 b)
{
  return a <= b;
}

/* { dg-final { scan-assembler-times "vcmpequq" 4 } } */
/* { dg-final { scan-assembler-times "vcmpgtsq" 4 } } */
/* { dg-final { scan-assembler-times "vcmpgtuq" 4 } } */
/* { dg-final { scan-assembler-times "xxlnor" 6 } } */
