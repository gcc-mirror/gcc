/* PR c/102989 */
/* { dg-do run { target bitint } } */
/* { dg-options "-fsanitize=signed-integer-overflow,shift -fsanitize-recover=signed-integer-overflow,shift" } */

__attribute__((noipa)) int
foo (_BitInt(15) *p, _BitInt(15) *q, int n)
{
  q[0] = p[0] + p[1];
  q[1] = p[2] - p[3];
  q[2] = p[4] * p[5];
  q[3] = p[6] / p[7];
  q[4] = p[8] << n;
  q[5] = p[9] >> n;
  return n;
}

int
main ()
{
  static _BitInt(15) p[] = {
    16382wb, 1wb,
    -16383wb, 1wb,
    127wb, 129wb,
    -16383wb - 1, 1wb,
    0wb, 1wb,
    16383wb, 1wb,
    -16383wb - 1, 1wb,
    127wb, 130wb,
    -16383wb - 1, -1wb,
    0wb, 1wb
  };
  _BitInt(15) q[6];
  int n = foo (p, q, 14);
  q[4] = p[8] << (n + 2);
  q[5] = p[9] >> (n + 2);
  foo (p + 10, q, 15);
}

/* The 16-bit type vs. 15-bit is small inaccuracy.  */
/* { dg-output "shift exponent 16 is too large for 16-bit type '_BitInt\\\(15\\\)'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*shift exponent 16 is too large for 16-bit type '_BitInt\\\(15\\\)'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*signed integer overflow: 16383 \\+ 1 cannot be represented in type '_BitInt\\\(15\\\)'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*signed integer overflow: -16384 - 1 cannot be represented in type '_BitInt\\\(15\\\)'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*signed integer overflow: 127 \\* 130 cannot be represented in type '_BitInt\\\(15\\\)'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*division of -16384 by -1 cannot be represented in type '_BitInt\\\(15\\\)'\[^\n\r]*(\n|\r\n|\r)" } */
/* This isn't accurate, but libubsan right now doesn't know it is 15-bit type rather than 16-bit.  */
/* { dg-output "\[^\n\r]*left shift of 0 by 15 places cannot be represented in type '_BitInt\\\(15\\\)'\[^\n\r]*(\n|\r\n|\r)" } */
/* The wording is totally incorrect, but at least it is diagnosed.  */
/* { dg-output "\[^\n\r]*left shift of 1 by 15 places cannot be represented in type '_BitInt\\\(15\\\)'" } */
