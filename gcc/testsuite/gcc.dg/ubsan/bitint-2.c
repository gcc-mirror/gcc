/* PR c/102989 */
/* { dg-do run { target { bitint128 && int128 } } } */
/* { dg-options "-fsanitize=signed-integer-overflow,shift -fsanitize-recover=signed-integer-overflow,shift" } */

__attribute__((noipa)) int
foo (_BitInt(125) *p, _BitInt(125) *q, int n)
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
  static _BitInt(125) p[] = {
    21267647932558653966460912964485513214wb, 1wb,
    -21267647932558653966460912964485513215wb, 1wb,
    4611686018427387903wb, 4611686018427387905wb,
    -21267647932558653966460912964485513215wb - 1, 1wb,
    0wb, 1wb,
    21267647932558653966460912964485513215wb, 1wb,
    -21267647932558653966460912964485513215wb - 1, 1wb,
    4611686018427387903wb, 4611686018427387906wb,
    -21267647932558653966460912964485513215wb - 1, -1wb,
    0wb, 1wb
  };
  _BitInt(125) q[6];
  int n = foo (p, q, 123);
  q[4] = p[8] << (n + 5);
  q[5] = p[9] >> (n + 5);
  foo (p + 10, q, 125);
}

/* The 128-bit type vs. 125-bit is small inaccuracy.  */
/* { dg-output "shift exponent 128 is too large for 128-bit type '_BitInt\\\(125\\\)'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*shift exponent 128 is too large for 128-bit type '_BitInt\\\(125\\\)'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*signed integer overflow: 0x0fffffffffffffffffffffffffffffff \\+ 1 cannot be represented in type '_BitInt\\\(125\\\)'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*signed integer overflow: 0xf0000000000000000000000000000000 - 1 cannot be represented in type '_BitInt\\\(125\\\)'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*signed integer overflow: 4611686018427387903 \\* 4611686018427387906 cannot be represented in type '_BitInt\\\(125\\\)'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*division of 0xf0000000000000000000000000000000 by -1 cannot be represented in type '_BitInt\\\(125\\\)'\[^\n\r]*(\n|\r\n|\r)" } */
/* This isn't accurate, but libubsan right now doesn't know it is 125-bit type rather than 128-bit.  */
/* { dg-output "\[^\n\r]*left shift of 0 by 125 places cannot be represented in type '_BitInt\\\(125\\\)'\[^\n\r]*(\n|\r\n|\r)" } */
/* The wording is totally incorrect, but at least it is diagnosed.  */
/* { dg-output "\[^\n\r]*left shift of 1 by 125 places cannot be represented in type '_BitInt\\\(125\\\)'" } */
