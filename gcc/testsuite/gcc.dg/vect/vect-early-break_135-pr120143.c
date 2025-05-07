/* { dg-do compile } */
/* { dg-add-options vect_early_break } */
/* { dg-additional-options "-O3 -fwhole-program" } */

short a;
extern _Bool b[][23];
short g = 6;
int v[4];
int x[3];
void c(short g, int v[], int x[]) {
  for (;;)
    for (unsigned y = 0; y < 023; y++) {
      b[y][y] = v[y];
      for (_Bool aa = 0; aa < (_Bool)g; aa = x[y])
        a = a > 0;
    }
}
int main() { c(g, v, x); }
