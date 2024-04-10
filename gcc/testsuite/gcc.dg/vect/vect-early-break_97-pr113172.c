/* { dg-do compile } */
/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */

int tswchp_2;
short cpy_buf[8];
void ts_endcmd() {
  int i = 0;
  for (; i < 8 && i < tswchp_2; i++)
    cpy_buf[i] = i;
}
