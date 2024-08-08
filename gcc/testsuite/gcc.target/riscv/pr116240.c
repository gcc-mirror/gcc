/* { dg-do compile } */
/* { dg-additional-options "-fwrapv -march=rv64imvxtheadcondmov_xventanacondops -mabi=lp64d" } */

int a, b;
void c() {
  int e = a >= 2 ? b : a;
  short d = e * 2;
  if (d)
    for (;;)
      ;
}

