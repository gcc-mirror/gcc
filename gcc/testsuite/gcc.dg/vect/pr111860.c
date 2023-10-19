/* { dg-do compile } */

int optimize_path_n, optimize_path_d;
int *optimize_path_d_0;
extern void path_threeOpt( long);
void optimize_path() {
  int i;
  long length;
  i = 0;
  for (; i <= optimize_path_n; i++)
    optimize_path_d = 0;
  i = 0;
  for (; i < optimize_path_n; i++)
    length += optimize_path_d_0[i];
  path_threeOpt(length);
}
