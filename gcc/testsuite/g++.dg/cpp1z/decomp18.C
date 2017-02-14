// { dg-do compile { target c++11 } }
// { dg-options "" }

struct A { char a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r; } a[64];

void
foo ()
{
  int z = 0;
  for (auto & [ b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s ] : a)	// { dg-warning "decomposition declaration only available with" "" { target c++14_down } }
    z += b + c + d + e + f + g + h + i + j + k + l + m + n + o + p + q + r + s;
}
