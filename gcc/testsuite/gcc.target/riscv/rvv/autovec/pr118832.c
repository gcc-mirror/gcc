/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zbb -mabi=lp64d -O3" } */

int *a;
void b(int *);
void c(int *g, short h) {
    int d[8], e[8];
    for (int f = 0; f < h; f++)
      d[f] = g[f] << 24 | (g[f] & 4278190080) >> 24;
    b(d);
    for (int f = 0; f < h; f++)
      a[f] = e[f] << 24 | (e[f] & 4278190080) >> 24;
}
