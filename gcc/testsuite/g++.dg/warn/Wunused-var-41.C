// PR c/44677
// { dg-do compile }
// { dg-options "-O2 -Wunused-but-set-variable" }

void baz (int);

template <typename T>
void
foo (void)
{
  T a = 0;		// { dg-warning "variable 'a' set but not used" }
  a = 1;
  T b = 0;		// { dg-warning "variable 'b' set but not used" }
  ++b;
  T c = 0;		// { dg-warning "variable 'c' set but not used" }
  c++;
  T d = 0;		// { dg-warning "variable 'd' set but not used" }
  --d;
  T e = 0;		// { dg-warning "variable 'e' set but not used" }
  e--;
  T f = 0;		// { dg-warning "variable 'f' set but not used" }
  f += 2;
  T g = 0;		// { dg-warning "variable 'g' set but not used" }
  g |= 2;
  T h = 0;		// { dg-warning "variable 'h' set but not used" }
  h -= 2;
  T i = 0;		// { dg-warning "variable 'i' set but not used" }
  i &= 2;
  T j = 0;		// { dg-warning "variable 'j' set but not used" }
  j ^= 2;
  T k = 0;		// { dg-warning "variable 'k' set but not used" }
  k *= 2;
  T l = 0;		// { dg-warning "variable 'l' set but not used" }
  l %= 2;
  T m = 0;		// { dg-warning "variable 'm' set but not used" }
  for (T n = 4; n < 10; n++, m++)
    baz (n);
}

template <typename T>
T
bar (void)
{
  T a = 0;
  T b = ++a;
  T c = 0;
  T d = --c;
  T e = 0;
  T f = e--;
  T g = 0;
  T h = g++;
  T i = 0;
  T j;
  j = i += 42;
  T k = 0;
  T l;
  l = k *= 4;
  T m = 0;
  T n;
  n = m |= 2;
  return b + d + f + h + j + l + n;
}

void
test ()
{
  foo <int> ();
  bar <int> ();
}
