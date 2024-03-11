/* { dg-do compile } */
/* { dg-options "-O2 -Wno-pointer-to-int-cast" } */
typedef char int8_t;
int8_t c_4, uli_5;
unsigned short us_6;
void func_1() {
  int uli_9;
  short ptr_16ptr_11 = (short) &uli_9;
  for (; us_6 <= 6;)
    if ((us_6 *= uli_9) < (uli_5 || 0) ?: ((c_4 = us_6) >= us_6) - uli_5)
      uli_9 = 9;
}
