/* { dg-do compile } */

int is_table[2][16];
int is_table_lsf[2][2][16];
void compute_stereo()
{
  int (*is_tab)[16];
  is_tab = is_table;
}
