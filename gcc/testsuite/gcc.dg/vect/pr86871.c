/* { dg-do compile } */

extern int b[];
extern int c[];
void g(int f) {
  for (; f; f++) {
    int d = 0;
    for (int e = -1; e <= 1; e++) {
      int a = f + e;
      if (a)
        d = *(c + a);
    }
    *(b + f) = d;
  }
 }
