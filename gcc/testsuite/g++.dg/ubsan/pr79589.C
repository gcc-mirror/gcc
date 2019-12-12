// PR sanitizer/79589
// { dg-do compile { target c++17 } }
// { dg-options "-fsanitize=undefined" }

struct A { char a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r; } a[64];

void
foo ()
{
  int z = 0;
  for (auto & [ b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s ] : a)
    z += b + c + d + e + f + g + h + i + j + k + l + m + n + o + p + q + r + s;
}
