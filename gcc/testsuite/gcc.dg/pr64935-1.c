/* PR rtl-optimization/64935 */
/* { dg-do compile } */
/* { dg-options "-std=gnu89 -Wno-shift-count-overflow -O2 -fcompare-debug" } */

int a[] = {}, b[] = {}, c[] = {}, d[] = {}, e[] = {}, f[] = {}, h[] = {};
int g[] = { 0 };
int i, l, s, w, x, y, z, t2, t3, t5;
unsigned long j, m, o, t4;
long k, n, p, q, r, t, u, v, t1;
fn1 ()
{
  int t6;
  for (; i; i++)
    {
      t5 = a[q] ^ b[p >> 1] ^ c[o >> 1 & 1] ^ d[n >> 1 & 1] ^ e[m >> 1 & 1]
           ^ f[l >> 1 & 1] ^ g[0] ^ h[j & 1];
      t4 = a[j] ^ b[q >> 1] ^ c[p] ^ d[o] ^ e[n] ^ f[m] ^ g[l >> 8] ^ h[k];
      t3 = a[k >> 1] ^ b[j & 5] ^ d[p >> 32] ^ e[o >> 4] ^ f[n >> 6]
           ^ g[m >> 8] ^ h[l];
      t2 = a[l >> 6] ^ b[k & 1] ^ c[j >> 1] ^ d[q >> 32] ^ e[p >> 4]
           ^ f[o >> 6] ^ g[n >> 8] ^ h[m & 1];
      t1 = a[m >> 6] ^ b[l & 1] ^ c[k & 15] ^ d[j >> 2] ^ e[q >> 4] ^ f[p >> 6]
           ^ g[o >> 8] ^ h[n & 1];
      z = a[n >> 56] ^ b[m & 15] ^ c[l & 15] ^ d[k >> 2] ^ e[j >> 4]
          ^ f[q >> 6] ^ g[p >> 8] ^ h[o & 1];
      y = a[o >> 56] ^ b[n & 15] ^ c[m >> 40] ^ d[l >> 2] ^ e[k >> 4]
          ^ f[j >> 6] ^ g[q >> 8] ^ h[p & 1];
      x = a[p >> 56] ^ b[o & 15] ^ c[n >> 40] ^ d[m >> 2] ^ e[l >> 4]
          ^ f[k >> 6] ^ g[j >> 8] ^ h[q & 1];
      q = j = t4;
      k = t3;
      l = t2;
      m = t1;
      n = z;
      o = y;
      p = a[t6] ^ b[0] ^ c[w] ^ d[v] ^ e[u] ^ f[t] ^ g[s] ^ h[r];
      t4 = a[r >> 1] ^ b[t6 & 1] ^ d[w >> 1] ^ e[v >> 1] ^ f[u >> 1]
           ^ g[t >> 1] ^ h[s];
      t3 = a[s >> 6] ^ b[r & 1] ^ c[t6 & 5] ^ d[0] ^ e[w >> 4] ^ f[v >> 6]
           ^ g[u >> 8] ^ h[t & 1];
      t2 = a[t >> 6] ^ b[s] ^ c[r & 15] ^ d[t6 >> 1] ^ e[0] ^ f[w >> 6]
           ^ g[v >> 8] ^ h[u & 1];
      t1 = a[u >> 6] ^ b[t & 15] ^ c[s & 5] ^ d[r >> 32] ^ e[t6 >> 4]
           ^ g[w >> 8] ^ h[v & 1];
      z = a[v >> 56] ^ b[u >> 48 & 1] ^ c[t >> 40 & 1] ^ d[s] ^ e[r >> 1 & 1]
          ^ f[t6 >> 1 & 1] ^ g[0] ^ h[w & 1] ^ z;
      t6 = t5;
      r = t4;
      s = 0;
      t = u = t1;
      v = z;
      w = y;
    }
}
