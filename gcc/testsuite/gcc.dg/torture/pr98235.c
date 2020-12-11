/* { dg-do compile } */
/* { dg-additional-options "-fallow-store-data-races" } */

char tcube[3][9];
int cur_move;
void perm_cube(void) {
  int i, j, k, tmp;
  for (; i < cur_move; i++)
    while (k-- >= 0)
      switch (j) {
      case 0:
        tmp = tcube[0][6];
        tcube[2][8] = tcube[0][8];
        tcube[0][8] = tmp;
        tmp = tcube[0][5];
        tcube[0][5] = tcube[1][8];
        tcube[1][8] = tcube[2][5];
        tcube[2][5] = tcube[1][2];
        tcube[1][2] = tcube[2][1];
        tcube[2][1] = tcube[1][0];
        tcube[0][6] = tmp;
        tmp = tcube[0][3];
        tcube[0][3] = tcube[1][0];
        tcube[1][0] = tcube[2][3];
        tcube[2][3] = tcube[1][6];
        tcube[1][6] = tmp;
        break;
      case 5:
        tmp = tcube[2][0];
        tcube[2][0] = tcube[2][2];
        tcube[2][2] = tcube[2][8];
        tcube[2][3] = tmp;
      }
}
