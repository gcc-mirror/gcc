/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -mrvv-vector-bits=zvl" } */

int a;
void b() {
  unsigned long c = 18446744073709551612UL;
d:
  --c;
  a ^= c;
  if (c)
    goto d;
}
