/* PR target/98657  */
/* { dg-do compile } */
/* { dg-options "-O3 -msve-vector-bits=256" } */
extern char a[];
void b(_Bool c[][18]) {
  int d;
  for (int e = 0; e < 23; e++)
    a[e] = 6 >> c[1][d];
}
