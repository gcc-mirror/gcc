/* { dg-do compile } */
/* { dg-options "" } */
struct str {};
struct {
  struct str b;
  float c[1];
  int d[1];
  float e[2];
  int f[1];
} a = {{}, 0, {0.5}, 0, 0, {0}};
