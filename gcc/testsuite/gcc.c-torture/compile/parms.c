/* { dg-skip-if "too many arguments in function call" { bpf-*-* } } */
/* { dg-additional-options "-std=gnu89" } */
#define alloca __builtin_alloca

x (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, x, y)
{
  foo (alloca (8));
  return a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t+u+v+x+y;
}
