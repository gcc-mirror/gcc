/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl256b -mrvv-vector-bits=zvl -mabi=lp64" } */

char b[13][13];
void c() {
  for (int d = 0; d < 13; ++d)
    for (int e = 0; e < 13; ++e)
      b[d][e] = e == 0 ? -98 : 38;
}



