// PR c/44677
// { dg-do compile }
// { dg-options "-O2 -Wunused-but-set-parameter" }

void baz (int);

template <typename T>
void
foo (T a,		// { dg-warning "parameter 'a' set but not used" }
     T b,		// { dg-warning "parameter 'b' set but not used" }
     T c,		// { dg-warning "parameter 'c' set but not used" }
     T d,		// { dg-warning "parameter 'd' set but not used" }
     T e,		// { dg-warning "parameter 'e' set but not used" }
     T f,		// { dg-warning "parameter 'f' set but not used" }
     T g,		// { dg-warning "parameter 'g' set but not used" }
     T h,		// { dg-warning "parameter 'h' set but not used" }
     T i,		// { dg-warning "parameter 'i' set but not used" }
     T j,		// { dg-warning "parameter 'j' set but not used" }
     T k,		// { dg-warning "parameter 'k' set but not used" }
     T l,		// { dg-warning "parameter 'l' set but not used" }
     T m)		// { dg-warning "parameter 'm' set but not used" }
{
  a = 1;
  ++b;
  c++;
  --d;
  e--;
  f += 2;
  g |= 2;
  h -= 2;
  i &= 2;
  j ^= 2;
  k *= 2;
  l %= 2;
  for (T n = 4; n < 10; n++, m++)
    baz (n);
}

template <typename T>
T
bar (T a, T b, T c, T d, T e, T f, T g, T h, T i, T j,
     T k, T l, T m, T n)
{
  b = ++a;
  d = --c;
  f = e--;
  h = g++;
  j = i += 42;
  l = k *= 4;
  n = m |= 2;
  return b + d + f + h + j + l + n;
}

void
test ()
{
  foo <int> (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  bar <int> (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
}
