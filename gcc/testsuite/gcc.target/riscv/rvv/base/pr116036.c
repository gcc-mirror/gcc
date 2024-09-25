/* { dg-do compile } */
/* { dg-options "-march=rv64idv -mabi=lp64d -O3" } */

int a[15][15];
void init() {
  for (int i_0 ; i_0 < 15 ; ++i_0)
    for (int i_1 = 0; i_1 < 15; ++i_1)
      a[i_0][i_1] = 1;
}

/* { dg-excess-errors "sorry, unimplemented: Currently the 'V' implementation requires the 'M' extension" } */
