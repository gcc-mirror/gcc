/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-additional-options "-O -ftree-vectorize" } */

unsigned char arr_7[9][3];
unsigned char (*main_arr_7)[3] = arr_7;
int main() {
  char arr_2[9];
  int arr_6[9];
  int x;
  unsigned i;
  for (i = 0; i < 9; ++i) {
    arr_2[i] = 21;
    arr_6[i] = 6;
  }
  for (i = arr_2[8] - 21; i < 2; i++)
    x = arr_6[i] ? (main_arr_7[8][i] ? main_arr_7[8][i] : 8) : (char)arr_6[i];
  if (x != 8)
    __builtin_abort ();
}

