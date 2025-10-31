/* { dg-do compile } */
/* PR tree-optimization/122497 */

/* This was ICEing during SCCP
   trying to simplify a reference back to the phi
   which was removed.  */

char g_2[1][2];
int g_4, g_5;
void main() {
  for (; g_4; g_4 -= 1)
    g_5 = g_2[g_4 + 2][g_4];
}
